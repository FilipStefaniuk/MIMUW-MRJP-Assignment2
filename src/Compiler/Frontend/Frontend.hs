{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Frontend.Frontend where
  
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Cont
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified AbsLatte as ABS
import LexLatte
import ParLatte
import ErrM

import Frontend.LLVM

data Variable = Variable {
    _variableType :: ABS.Type Loc,
    _variableAddress :: Register
}

data Class = Class {
    _className :: ABS.Ident,
    _classParent :: Maybe ABS.Ident,
    _classFields :: Map.Map ABS.Ident (ABS.Type Loc),
    _classMethods :: Map.Map ABS.Ident (ABS.FunDef Loc)
}

data GenMState = GenMState {
    _nextUniqueId :: !Integer,
    _localVariables :: Seq.Seq Alloc,
    _currentBlocks :: Map.Map Label Block,
    _currentBlockLabel :: Label,
    _arguments :: Seq.Seq Register,
    _program :: Program
}

data GenMEnv = GenMEnv {
    _currentFunction :: Maybe (ABS.FunDef Loc),
    _currentClass :: Maybe Class,
    _variables :: Map.Map ABS.Ident Variable,
    _blockVariables :: Set.Set ABS.Ident,
    _functions :: Map.Map ABS.Ident (ABS.FunDef Loc),
    _classes :: Map.Map ABS.Ident Class
}

type Loc = Maybe (Int, Int)

data GenMError = GenMError String
    deriving Show

instance Error GenMError where
    noMsg = GenMError "internal error"
    strMsg = GenMError

type GenM m = (MonadState GenMState m, MonadReader GenMEnv m, MonadError GenMError m, MonadIO m)

transBlock :: GenM m => ABS.Block Loc -> m ()
transBlock (ABS.Blk _ stmts) = do
    variables <- asks _variables
    blockVariables <- asks _blockVariables
    local (\env -> env{_blockVariables = Set.empty}) $ runContT (forM_ stmts collectDecl) (\_ -> forM_ stmts transStmt)
  where
    collectDecl :: GenM m => ABS.Stmt Loc -> ContT () m ()
    collectDecl (ABS.Decl _ type_ items) = forM_ items $ \item -> do
        ident <- case item of
            ABS.NoInit _ ident -> return ident 
            ABS.Init _ ident expr -> return ident
        asks (Set.member ident . _blockVariables) >>= \case
            True -> lift . throwError $ GenMError "error"
            False -> ContT $ \next -> do
                reg <- emitAlloc (transType type_)
                local (\env -> env{
                    _blockVariables = Set.insert ident $ _blockVariables env,
                    _variables = Map.insert ident (Variable type_ reg) $ _variables env
                }) $ next ()
    collectDecl _ = return ()

transStmt :: GenM m => ABS.Stmt Loc -> m ()
transStmt (ABS.Empty _) = return ()
transStmt (ABS.BStmt _ block) = transBlock block
transStmt (ABS.SExp _ expr) = void $ transExpr expr
transStmt (ABS.Decl _ _ items) = forM_ items transItem
transStmt (ABS.Ass _ lval expr) = do
    (type_, reg) <- transLVal lval
    (type_, operand) <- transExpr expr
    -- TODO: check types
    emitInstr $ InstrStore operand reg 
    
transStmt (ABS.Incr ln lval) = transStmt $ ABS.Ass ln lval $ ABS.EAdd ln (ABS.EVar ln lval) (ABS.Plus ln) (ABS.ELitInt ln 1)
transStmt (ABS.Decr ln lval) = transStmt $ ABS.Ass ln lval $ ABS.EAdd ln (ABS.EVar ln lval) (ABS.Minus ln) (ABS.ELitInt ln 1)

transStmt (ABS.Ret _ expr) = do
    (type_, operand) <- transExpr expr
    -- TODO: check types
    emitEnd $ BlockEndReturn operand

transStmt (ABS.VRet _) = emitEnd BlockEndReturnVoid

-- Sprawdzać poprawność skróconych wyrażeń
-- co jeśli jest return w bloku ???
transStmt (ABS.Cond _ expr stmt) = do
    (type_, operand) <- transExpr expr
    case operand of
        ConstBool True -> local (\env -> env{_blockVariables = Set.empty}) $ transStmt stmt
        ConstBool False -> return ()
        _ -> do
            begin <- gets _currentBlockLabel
            trueBlock <- newBlock
            modify $ \s -> s{_currentBlockLabel = trueBlock}
            local (\env -> env{_blockVariables = Set.empty}) $ transStmt stmt
            end <- newBlock
            emitEnd $ BlockEndBranch end
            modify $ \s -> s{_currentBlockLabel = begin}
            emitEnd $ BlockEndBranchCond operand trueBlock end
            modify $ \s -> s{_currentBlockLabel = end}

transStmt (ABS.CondElse _ expr stmt1 stmt2) = do
    (type_, operand) <- transExpr expr
    case operand of
        ConstBool True -> local (\env -> env{_blockVariables = Set.empty}) $ transStmt stmt1 
        ConstBool False ->  local (\env -> env{_blockVariables = Set.empty}) $ transStmt stmt2
        _ -> do
            begin <- gets _currentBlockLabel
            trueBlock <- newBlock
            modify $ \s -> s{_currentBlockLabel = trueBlock}
            local (\env -> env{_blockVariables = Set.empty}) $ transStmt stmt1
            falseBlock <- newBlock
            modify $ \s -> s{_currentBlockLabel = falseBlock}
            local (\env -> env{_blockVariables = Set.empty}) $ transStmt stmt2
            end <- newBlock
            emitEnd $ BlockEndBranch end
            modify $ \s -> s{_currentBlockLabel = trueBlock}
            emitEnd $ BlockEndBranch end
            modify $ \s -> s{_currentBlockLabel = begin}
            emitEnd $ BlockEndBranchCond operand trueBlock falseBlock
            modify $ \s -> s{_currentBlockLabel = end}

transStmt (ABS.While _ expr stmt) = do
    cond <- newBlock
    emitEnd $ BlockEndBranch cond
    modify $ \s -> s{_currentBlockLabel = cond}
    (type_, operand) <- transExpr expr
    case operand of
        ConstBool True -> do
            local (\env -> env{_blockVariables = Set.empty}) $ transStmt stmt
            emitEnd $ BlockEndBranch cond
            end <- newBlock
            modify $ \s -> s{_currentBlockLabel = end}
        _ -> do
            body <- newBlock
            modify $ \s -> s{_currentBlockLabel = body}
            local (\env -> env{_blockVariables = Set.empty}) $ transStmt stmt
            emitEnd $ BlockEndBranch cond
            end <- newBlock
            modify $ \s -> s{_currentBlockLabel = cond}
            emitEnd $ BlockEndBranchCond operand body end
            modify $ \s -> s{_currentBlockLabel = end}

--   For _ type_ ident expr stmt -> failure x

transItem :: GenM m => ABS.Item Loc -> m ()
transItem (ABS.NoInit _ ident) = return ()
transItem (ABS.Init _ ident expr) = do
    (type_, operand) <- transExpr expr
    return ()

transType :: ABS.Type Loc -> Type
transType (ABS.Bool _) = Size1
transType (ABS.Int _) = Size32
-- Void _ -> failure x
-- Arr _ type_ -> failure x
-- Obj _ ident -> failure x
-- Null _ -> failure x
-- Str _ -> failure x

transExpr :: GenM m => ABS.Expr Loc -> m (ABS.Type Loc, Operand)
transExpr (ABS.ELitInt _ integer) = return (ABS.Int Nothing, ConstInt integer)
transExpr (ABS.ELitTrue _) = return (ABS.Bool Nothing, ConstBool True)
transExpr (ABS.ELitFalse _) = return (ABS.Bool Nothing, ConstBool False)
transExpr (ABS.ENull _) = return (ABS.Null Nothing, Null)
transExpr (ABS.EVar _ lval) = do
    (type_, reg@(Register _ ty)) <- transLVal lval
    uniqueId <- getNextUniqueId
    reg' <- Register uniqueId <$> dereference ty
    emitInstr $ InstrLoad reg' reg 
    return (type_, Reg reg')
  where
    dereference :: GenM m => Type -> m Type
    dereference (Ptr type_) = return type_
    dereference _ = throwError $ GenMError "error"

--   EString _ string -> failure x

transExpr (ABS.ECall _ ident exprs) = (asks $ (Map.lookup ident) . _functions) >>= \case
    Nothing -> throwError $ GenMError "error"
    Just (ABS.FunDef _ type_ _ args _) -> do
        exprs <- mapM transExpr exprs
        case type_ of
            ABS.Void _ -> do
                emitInstr $ InstrVoidCall (map snd exprs)
                return (type_, Null)

--   EMetCall _ expr ident exprs -> failure x
--   ENewObj _ ident -> failure x
--   ENewArr _ type_ expr -> failure x
--   Neg _ expr -> failure x
--   Not _ expr -> failure x

transExpr (ABS.EMul _ expr1 mulop expr2) = do
    (type1, operand1) <- transExpr expr1
    (type2, operand2) <- transExpr expr2
    case (operand1, mulop, operand2) of
        (ConstInt int1, ABS.Times _, ConstInt int2) -> return (type1, ConstInt $ int1 * int2)
        (ConstInt int1, ABS.Div _, ConstInt int2) -> return (type1, ConstInt $ int1 `div` int2)
        (ConstInt int1, ABS.Mod _, ConstInt int2) -> return (type1, ConstInt $ int1 `mod` int2)
        _ -> do
            reg <- flip Register Size32 <$> getNextUniqueId
            emitInstr $ InstrBinOp reg (transMulOp mulop) operand1 operand2
            return (type1, Reg reg)
  where
    transMulOp :: ABS.MulOp Loc -> Op
    transMulOp (ABS.Times _) = Times
    transMulOp (ABS.Div _) = Div
    transMulOp (ABS.Mod _) = Mod

transExpr (ABS.EAdd _ expr1 addop expr2) = do
    (type1, operand1) <- transExpr expr1
    (type2, operand2) <- transExpr expr2
    case (operand1, addop, operand2) of
        (ConstInt int1, ABS.Plus _, ConstInt int2) -> return (type1, ConstInt $ int1 + int2)
        (ConstInt int1, ABS.Minus _, ConstInt int2) -> return (type1, ConstInt $ int1 - int2)
        _ -> do
            reg <- flip Register Size32 <$> getNextUniqueId
            emitInstr $ InstrBinOp reg (transAddOp addop) operand1 operand2
            return (type1, Reg reg)
  where
    transAddOp :: ABS.AddOp Loc -> Op
    transAddOp (ABS.Plus _) = Plus
    transAddOp (ABS.Minus _) = Minus

transExpr (ABS.ERel _ expr1 relop expr2) = do
    (type1, operand1) <- transExpr expr1
    (type2, operand2) <- transExpr expr2
    case (operand1, relop, operand2) of
        (ConstInt int1, relop, ConstInt int2) -> return (ABS.Bool Nothing, ConstBool $ computeRelop relop int1 int2)
        (ConstBool bool1, relop, ConstBool bool2) -> return (ABS.Bool Nothing, ConstBool $ computeRelop relop bool1 bool2)
        _ -> do
            reg <- flip Register Size1 <$> getNextUniqueId
            emitInstr $ InstrCmp reg (transRelOp relop) operand1 operand2
            return (ABS.Bool Nothing, Reg reg)
  where 
    computeRelop :: (Ord a, Eq a) => ABS.RelOp Loc -> a -> a -> Bool
    computeRelop (ABS.LTH _) lhs rhs = lhs < rhs
    computeRelop (ABS.LE _) lhs rhs = lhs <= rhs
    computeRelop (ABS.GTH _) lhs rhs = lhs > rhs
    computeRelop (ABS.GE _) lhs rhs = lhs >= rhs
    computeRelop (ABS.EQU _) lhs rhs = lhs == rhs
    computeRelop (ABS.NE _) lhs rhs = lhs /= rhs

    transRelOp :: ABS.RelOp Loc -> Cond
    transRelOp (ABS.LTH _) = LTH
    transRelOp (ABS.LE _) = LE
    transRelOp (ABS.GTH _) = GTH
    transRelOp (ABS.GE _) = GE
    transRelOp (ABS.EQU _) = EQU
    transRelOp (ABS.NE _) = NE

-- TODO Sprawdzać typy skróconych wyrażeń !!!
transExpr (ABS.EAnd _ expr1 expr2) = do
    (type1, operand1) <- transExpr expr1
    case operand1 of
        ConstBool False -> return (type1, operand1)
        ConstBool True -> do
            (type2, operand2) <- transExpr expr2
            return (type2, operand2)
        _ -> do
            first <- gets _currentBlockLabel
            mid <- newBlock
            modify $ \s -> s{_currentBlockLabel = mid}
            (type2, operand2) <- transExpr expr2
            end <- newBlock
            emitEnd $ BlockEndBranch end
            modify $ \s -> s{_currentBlockLabel = first}
            emitEnd $ BlockEndBranchCond operand1 mid end
            modify $ \s -> s{_currentBlockLabel = end}
            reg <- flip Register Size1 <$> getNextUniqueId
            emitPhi $ Phi reg [PhiBranch first (ConstBool False), PhiBranch mid operand2]
            return (type1, Reg reg)

transExpr (ABS.EOr _ expr1 expr2) = do
    (type1, operand1) <- transExpr expr1
    case operand1 of
        ConstBool True -> return (type1, operand1)
        ConstBool False -> do
            (type2, operand2) <- transExpr expr2
            return (type2, operand2)
        _ -> do
            first <- gets _currentBlockLabel
            mid <- newBlock
            modify $ \s -> s{_currentBlockLabel = mid}
            (type2, operand2) <- transExpr expr2
            end <- newBlock
            emitEnd $ BlockEndBranch end
            modify $ \s -> s{_currentBlockLabel = first}
            emitEnd $ BlockEndBranchCond operand1 end mid
            modify $ \s -> s{_currentBlockLabel = end}
            reg <- flip Register Size1 <$> getNextUniqueId
            emitPhi $ Phi reg [PhiBranch first (ConstBool True), PhiBranch mid operand2]
            return (type1, Reg reg)


transLVal :: GenM m => ABS.LVal Loc -> m (ABS.Type Loc, Register)
transLVal (ABS.LVar _ ident) = asks (Map.lookup ident . _variables) >>= \case
    Nothing -> throwError $ GenMError "error"
    Just var -> return (_variableType var, _variableAddress var)
--   LArr _ expr1 expr2 -> failure x
--   LAttr _ expr ident -> failure x
--   LSelf _ -> failure x

transFunDef :: GenM m => ABS.FunDef Loc -> m ()
transFunDef fun@(ABS.FunDef _ type_ (ABS.Ident str) args block) = do
    modify $ \s -> s {
        _nextUniqueId = 1,
        _localVariables = Seq.empty,
        _currentBlocks = Map.fromList [(Label 0, Block (Label 0) Seq.empty Seq.empty BlockEndNone)], 
        _currentBlockLabel = Label 0
    }
    local (\env -> env{
        _currentFunction = Just fun
    }) $ runContT (forM_ args transArg) (\_ -> transBlock block)

    arguments <- gets _arguments
    vars <- gets _localVariables
    blocks <- gets _currentBlocks
    emitFunction $ FunctionDef {
        _functionType = Size32,
        _functionName = Ident str,
        _functionArgs = arguments,
        _localVars = vars,
        _blocks = blocks
    }
  where 
    transArg :: GenM m => ABS.Arg Loc -> ContT () m ()
    transArg (ABS.Ar _ type_ ident) = asks (Set.member ident . _blockVariables) >>= \case
        True -> lift . throwError $ GenMError "error"
        False -> ContT $ \next -> do
            reg <- emitAlloc Size32 --TODO Add other types
            modify $ \s -> s{_arguments = reg Seq.<| _arguments s}
            local (\env -> env{
                _blockVariables = Set.insert ident $ _blockVariables env,
                _variables = Map.insert ident (Variable type_ reg) $ _variables env
            }) $ next ()

transTopDef :: GenM m => ABS.TopDef Loc -> m ()
transTopDef (ABS.TopFunDef _ fundef) = transFunDef fundef
    -- ClassDef _ ident classitemdefs -> failure x
    -- ClassExtDef _ ident1 ident2 classitemdefs -> failure x

transProgram :: GenM m => ABS.Program Loc -> m ()
transProgram (ABS.Prog _ topdefs) = runContT (forM_ topdefs collectTopDef) (\_ -> forM_ topdefs transTopDef)
  where 
    collectTopDef :: GenM m => ABS.TopDef Loc -> ContT () m ()
    collectTopDef (ABS.TopFunDef _ fundef) = collectFunDef fundef
    
    collectFunDef :: GenM m => ABS.FunDef Loc -> ContT () m ()
    collectFunDef fun@(ABS.FunDef _ type_ ident args block) = (lift . asks $ (Map.lookup ident) . _functions) >>= \case
        Just fun -> lift . throwError $ GenMError "error"
        Nothing -> ContT $ \next -> local (\env -> env{_functions = Map.insert ident fun $ _functions env}) $ next ()


gen :: ABS.Program Loc -> ErrorT GenMError IO Program
gen prog = _program <$> execStateT (runReaderT (transProgram prog) initGenMEnv) initGenMState
  where
    initGenMState :: GenMState
    initGenMState = GenMState {
        _nextUniqueId = 0,
        _localVariables = Seq.empty,
        _currentBlockLabel = Label 0,
        _currentBlocks = Map.empty,
        _program = Program Seq.empty,
        _arguments = Seq.empty
    }

    initGenMEnv :: GenMEnv
    initGenMEnv = GenMEnv{
        _currentFunction = Nothing,
        _currentClass = Nothing,
        _variables = Map.empty,
        _blockVariables = Set.empty,
        _functions = Map.empty,
        _classes = Map.empty
    }

-- transClassItemDef :: Show a => ClassItemDef a -> Result
-- transClassItemDef x = case x of
--     AttrDef _ type_ ident -> failure x
--     MethodDef _ fundef -> failure x

newBlock :: GenM m => m Label
newBlock = do
    (UniqueId int) <- getNextUniqueId
    label <- return $ Label int
    modify $ \s -> s{_currentBlocks = Map.insert label (Block label Seq.empty Seq.empty BlockEndNone) (_currentBlocks s)}
    return label

getNextUniqueId :: GenM m => m UniqueId
getNextUniqueId = do
    uniqueId <- gets _nextUniqueId
    modify $ \s -> s{_nextUniqueId = uniqueId + 1}
    return $ UniqueId uniqueId    

emitFunction :: GenM m => FunctionDef -> m ()
emitFunction funcDef = do
    modify (\s -> s{_program = (_program s) {
        _functionDefs = funcDef Seq.<| _functionDefs (_program s)
    }})

emitAlloc :: GenM m => Type -> m Register
emitAlloc type_ = do
    uniqueId <- getNextUniqueId
    reg <- return $ Register uniqueId $ Ptr type_
    modify $ \s -> s{_localVariables = (Alloc reg) Seq.<| _localVariables s}
    return reg

emitInstr :: GenM m => Instruction -> m ()
emitInstr instr = do
    label <- gets _currentBlockLabel
    block <- gets $ (Map.! label) . _currentBlocks
    modify (\s -> s{_currentBlocks = Map.insert label (block{_blockBody= instr Seq.<| _blockBody block}) $ _currentBlocks s})

emitPhi :: GenM m => Phi -> m ()
emitPhi phi = do
    label <- gets _currentBlockLabel
    block <- gets $ (Map.! label) . _currentBlocks
    modify (\s -> s{_currentBlocks = Map.insert label (block{_blockPhi= phi Seq.<| _blockPhi block}) $ _currentBlocks s})

emitEnd :: GenM m => BlockEnd -> m ()
emitEnd end = do
    label <- gets _currentBlockLabel
    block <- gets $ (Map.! label) . _currentBlocks
    modify (\s -> s{_currentBlocks = Map.insert label (block{_blockEnd=end}) $ _currentBlocks s})








-- ---- Constants ------------------------------------------------------------------------------------

-- initEnv :: Env
-- initEnv = Env {
--     variables = M.empty,
--     classes = M.empty,
--     functions = M.fromList [(ident, f) | f@(Fun ident _ _) <- builtins]
-- }

-- funMain :: Ident
-- funMain = Ident "main"

-- builtins :: [Function]
-- builtins = [
--     (Fun (Ident "printInt") TVoid [TInt]),
--     (Fun (Ident "printString") TVoid [TStr]),
--     (Fun (Ident "error") TVoid []),
--     (Fun (Ident "readInt") TInt []),
--     (Fun (Ident "readString") TStr [])]

-- --------------------------------------------------------------------------------------------------

-- checkProgram :: String -> IO (Either CompilerError (Program Loc))
-- checkProgram input = runErrorT $ do
--     program <- parseProgram input
--     -- env <- collectTopDefs program
--     -- typeCheckProgram program env
--     return program

-- -- ----- Parse program -------------------------------------------------------------------------------










































-- ----- Collect top level definitions ---------------------------------------------------------------

-- collectTopDefs :: Program Loc -> ErrorT CompilerError IO Env
-- collectTopDefs (Prog _ topdefs) = do 
--     env <- execStateT (mapM_ collectTopDef topdefs) initEnv
--     validateMain env
--     return env

-- validateMain :: Env -> ErrorT CompilerError IO ()
-- validateMain env = case M.lookup funMain $ functions env of
--     Nothing -> throwError $ noMainError
--     Just (Fun _ type_ args) -> unless (type_ == TInt && null args) $ throwError noMainError

-- collectTopDef :: TopDef Loc -> StateT Env (ErrorT CompilerError IO) ()
-- collectTopDef (ClassDef nr ident items) = do
--     classes <- gets classes
--     when (M.member ident classes) . throwError $  multipleClassDeclarationError nr ident
--     collectClass ident Nothing items

-- collectTopDef (ClassExtDef nr ident1 ident2 items) = do 
--     classes <- gets classes
--     when (M.member ident1 classes) . throwError $ multipleClassDeclarationError nr ident1
--     collectClass ident1 (Just ident2) items

-- collectTopDef (TopFunDef nr fundef) = do
--     functions <- gets functions
--     fun@(Fun ident _ _) <- lift $ extractFunction fundef
--     when (M.member ident functions) . throwError $ multipleFunctionDeclarationError nr ident
--     modify $ \env -> env {functions = M.insert ident fun functions}
    
-- collectClass :: Ident -> Maybe Ident -> [ClassItemDef Loc] -> StateT Env (ErrorT CompilerError IO) ()
-- collectClass ident@(Ident str) parent items = do
--     class_ <- lift . execStateT (mapM_ collectClassItemDef items) $ Class parent M.empty M.empty
--     modify $ \env -> env {classes = M.insert ident class_ $ classes env}
--     `catchError` (\err -> throwError $ InClassError str err)

-- collectClassItemDef :: ClassItemDef Loc -> StateT Class (ErrorT CompilerError IO) ()
-- collectClassItemDef (AttrDef nr type_ ident) = do
--     fields <- gets fields
--     when (M.member ident fields) . throwError $ multipleFieldDeclarationError nr ident
--     modify $ \class_ -> class_ {fields = M.insert ident (ttype type_) fields}

-- collectClassItemDef (MethodDef nr fundef) = do
--     methods <- gets methods
--     fun@(Fun ident@(Ident str) _ _) <- lift $ extractFunction fundef
--     when (M.member ident methods) . throwError $ multipleMethodDeclarationError nr ident
--     modify $ \class_ -> class_ {methods = M.insert ident fun methods}

-- extractFunction :: FunDef Loc -> (ErrorT CompilerError IO) Function
-- extractFunction fun@(FunDef _ type_ ident@(Ident str) args block) = do 
--     types <- evalStateT (mapM extractArg args) S.empty
--     return $ Fun ident (ttype type_) types
--     `catchError` \err -> throwError $ InFunctionError str err 

-- extractArg :: Arg Loc -> StateT (S.Set Ident) (ErrorT CompilerError IO) TType
-- extractArg (Ar nr type_ ident) = do
--     args <- get
--     when (S.member ident args) . throwError $ multipleArgumentDeclarationError nr ident
--     modify $ S.insert ident
--     return $ ttype type_

-- ----- Type check top level difinitions -------------------------------------------------------------

-- typeCheckProgram :: Program Loc -> Env -> ErrorT CompilerError IO ()
-- typeCheckProgram (Prog _ topdefs) env = runReaderT (mapM_ typeCheckTopDef topdefs) $ Ctxt 0 Nothing TVoid env

-- typeCheckTopDef :: TopDef Loc -> ReaderT Context (ErrorT CompilerError IO) ()
-- typeCheckTopDef (TopFunDef _ fundef) = typeCheckFunDef fundef
-- typeCheckTopDef (ClassExtDef nr ident1 ident2 items) = typeCheckTopDef $ ClassDef nr ident1 items
-- typeCheckTopDef (ClassDef _ ident@(Ident str) items) = do
--     env' <- evalStateT (getClassEnv $ Just ident) S.empty
--     local (\ctxt -> ctxt { level = level ctxt + 1, env = env' {
--       functions = M.union (functions . env $ ctxt) $ functions env'
--     }}) $ mapM_ typeCheckClassItemDef items
--     `catchError` \err -> throwError $ InClassError str err

-- typeCheckClassItemDef :: ClassItemDef Loc -> ReaderT Context (ErrorT CompilerError IO) ()
-- typeCheckClassItemDef (MethodDef _ fundef) = typeCheckFunDef fundef
-- typeCheckClassItemDef (AttrDef nr type_ ident) = 
--     when ((ttype type_) == TVoid) . throwError $ voidFieldError nr

-- typeCheckFunDef :: FunDef Loc -> ReaderT Context (ErrorT CompilerError IO) ()
-- typeCheckFunDef (FunDef nr type_ ident@(Ident str) args block) = do
--     type_ <- typeCheckType type_
--     args <- fmap M.fromList $ mapM typeCheckArg args
--     vars <- asks $ variables . env
--     retStm <- local (\ctxt -> ctxt { 
--         retType = type_, 
--         env = (env ctxt) {variables = M.union vars args }}) 
--         $ execStateT (runContT (typeCheckBlock block) return) False
--     unless (type_ == TVoid || retStm) . throwError $ noReturnStmtError
--     `catchError` \err -> throwError $ InFunctionError str err 
    
-- typeCheckArg :: Arg Loc -> ReaderT Context (ErrorT CompilerError IO) (Ident, Variable)
-- typeCheckArg (Ar nr type_ ident) = do
--     type_ <- typeCheckType type_
--     when (type_ == TVoid) . throwError $ voidArgumentError nr
--     lvl <- asks level
--     return (ident, Var lvl type_)

-- getClassEnv :: Maybe Ident -> StateT (S.Set Ident) (ReaderT Context (ErrorT CompilerError IO)) Env
-- getClassEnv Nothing = return $ Env M.empty M.empty M.empty
-- getClassEnv (Just ident) = do
--     descendants <- get
--     when (S.member ident descendants) . throwError $ inheritenceCycleError
--     class_ <- asks $ (M.lookup ident . classes) . env 
--     case class_ of
--         Nothing -> throwError $ noAncestorClassError ident
--         Just class_ -> do
--             modify $ S.insert ident
--             env' <- getClassEnv . parent $ class_
--             lift . local (\ctxt -> ctxt{env = env'}) . mapM_ validateOverride . M.elems $ methods class_
--             lvl <- asks $ level
--             return env' {
--                 variables = M.union (variables env') $ M.map (Var lvl) (fields class_),
--                 functions = M.union (functions env') (methods class_)
--             }

-- validateOverride :: Function -> ReaderT Context (ErrorT CompilerError IO) ()
-- validateOverride (Fun ident ttype ttypes) = do
--     functions <- asks $ functions . env
--     case M.lookup ident functions of
--         Nothing -> return ()
--         (Just (Fun ident2 ttype2 ttypes2)) -> 
--             unless (ttype == ttype2 && ttypes == ttypes2) . throwError $ overrideError

-- ----- Type check Statements ------------------------------------------------------------------------
  
-- typeCheckBlock :: Block Loc -> ContT () (StateT Bool (ReaderT Context (ErrorT CompilerError IO))) ()
-- typeCheckBlock (Blk _ stmts) = do
--     lift . local (\ctxt -> ctxt {level = level ctxt + 1}) $ runContT (mapM_ typeCheckStmt stmts) return
--     ContT $ \next -> next ()

-- typeCheckStmt :: Stmt Loc -> ContT () (StateT Bool (ReaderT Context (ErrorT CompilerError IO))) ()
-- typeCheckStmt (BStmt _ block) = typeCheckBlock block
-- typeCheckStmt (Empty _) = return ()
  
-- typeCheckStmt (Ret nr expr) = lift $ do
--     env <- asks env
--     retType <- asks retType
--     exprType <- lift $ typeCheckExpr expr
--     compatibleTypes <- lift $ isCompatible retType exprType
--     unless (compatibleTypes) . throwError $ wrongReturnTypeError nr retType exprType
--     modify $ const True
  
-- typeCheckStmt (VRet nr) = lift $ do
--     retType <- asks retType
--     unless (retType == TVoid) . throwError $ voidRetInNonVoidError
--     modify $ const True

-- typeCheckStmt (Ass nr lval expr) = lift $ do
--     env <- asks env
--     lvalType <- lift $ typeCheckLVal lval
--     exprType <- lift $ typeCheckExpr expr
--     compatibleTypes <- lift $ isCompatible lvalType exprType
--     unless (compatibleTypes) . throwError $ wrongTypesError nr lvalType exprType

-- typeCheckStmt (Decl _ type_ items) = do
--     lvl <- asks level
--     env <- asks env
--     type_ <- lift . lift $ typeCheckType type_ 
--     env' <- lift . lift $ runReaderT (execStateT (mapM_ typeCheckItem items) env) (type_, lvl)
--     ContT $ \next -> local (\ctxt -> ctxt{env = env'}) $ next ()

-- typeCheckStmt (Cond nr expr stmt) = typeCheckStmt (CondElse nr expr stmt (Empty Nothing))      
-- typeCheckStmt (CondElse nr expr stmt1 stmt2) = lift $ do
--     env <- asks env
--     exprType <- lift $ typeCheckExpr expr
--     unless (exprType == TBool) . throwError $ wrongCondExprType nr
--     retStm1 <- lift . local (\ctxt -> ctxt {level = level ctxt + 1}) $ execStateT (runContT (typeCheckStmt stmt1) return) False
--     retStm2 <- lift . local (\ctxt -> ctxt {level = level ctxt + 1}) $ execStateT (runContT (typeCheckStmt stmt2) return) False
--     modify $ \ret -> ret || retStm1 && retStm2

-- typeCheckStmt (While nr expr stmt) = lift $ do
--     env <- asks env
--     exprType <- lift $ typeCheckExpr expr
--     unless (exprType == TBool) . throwError $ wrongCondExprType nr
--     local (\ctxt -> ctxt {level = level ctxt + 1}) $ runContT (typeCheckStmt stmt) return

-- typeCheckStmt (Decr nr lval) = typeCheckStmt (Incr nr lval)
-- typeCheckStmt (Incr nr lval) = lift $ do
--     env <- asks env
--     type_ <- lift $ typeCheckLVal lval
--     unless (type_ == TInt) . throwError $ incrDecrError nr

-- typeCheckStmt (For nr type_ ident expr stmt) = lift $ do
--     lvl <- asks level
--     env <- asks env
--     type_ <- lift $ typeCheckType type_
--     expType <- lift $ typeCheckExpr expr
--     case getArrElementType expType of
--         Nothing -> throwError $ iterationError nr expType
--         Just arrElType -> do
--             compatibleTypes <- lift $ isCompatible type_ arrElType
--             unless (compatibleTypes) . throwError $ wrongTypesError nr type_ arrElType
--             ctxt' <- asks $ \ctxt -> ctxt{level = lvl + 2, env = env{variables = M.insert ident (Var (lvl + 1) type_) $ variables env}}
--             retStm <- lift . local (const ctxt') $ execStateT (runContT (typeCheckStmt stmt) return) False
--             modify $ \ret -> ret || retStm

-- typeCheckStmt (SExp _ expr) = lift $ do
--     env <- asks env
--     lift . void $ typeCheckExpr expr

-- typeCheckItem :: Item Loc -> StateT Env (ReaderT (TType, Integer) (ReaderT Context (ErrorT CompilerError IO))) ()
-- typeCheckItem (Init nr ident expr) = do
--     env <- get
--     type_ <- asks fst
--     exprType <- lift . lift $ typeCheckExpr expr
--     compatibleTypes <- lift . lift $ isCompatible type_ exprType
--     lift . unless (compatibleTypes) . throwError $ wrongTypesError nr type_ exprType
--     typeCheckItem (NoInit nr ident)

-- typeCheckItem (NoInit nr ident) = do
--     (type_, lvl) <- ask
--     var <- gets $ M.lookup ident . variables
--     case var of
--         Just var -> when (varDepth var == lvl) . throwError $ variableRedefinitionError nr ident
--         Nothing -> return ()
--     modify $ \env -> env {variables = M.insert ident (Var lvl type_) $ variables env}
  

-- -------------------------------------- Exprs ------------------------------------------------------

-- typeCheckLVal :: LVal Loc -> ReaderT Context (ErrorT CompilerError IO) TType
-- typeCheckLVal (LVar nr ident) = do
--     variables <- asks $ variables . env 
--     unless (M.member ident variables) . throwError $ undeclaredIdentifierError nr ident
--     return . varType $ (variables M.! ident)

-- typeCheckLVal (LArr nr expr1 expr2) = do
--     exprType1 <- typeCheckExpr expr1
--     exprType2 <- typeCheckExpr expr2
--     unless (exprType2 == TInt) . throwError $ arrayIndexTypeError nr
--     case getArrElementType exprType2 of
--         Nothing -> throwError $ arrayTypeRequiredError nr
--         Just type_ -> return type_

-- typeCheckLVal (LAttr nr expr ident) = do
--     type_ <- typeCheckExpr expr
--     case type_ of
--         TObj classIdent -> do
--             variables <- fmap variables $ evalStateT (getClassEnv $ Just classIdent) S.empty
--             case M.lookup ident variables of
--                 Nothing -> throwError $ noFieldError nr classIdent ident
--                 Just var -> return $ varType var
--         _ -> throwError $ classTypeError nr

-- typeCheckLVal (LSelf nr) = do
--     self <- asks self
--     case self of
--         Nothing -> throwError $ thisOutsideClassError nr
--         Just ident -> return $ TObj ident

-- typeCheckExpr ::  Expr Loc -> ReaderT Context (ErrorT CompilerError IO) TType
-- typeCheckExpr (ELitTrue _) = return TBool

-- typeCheckExpr (ELitFalse _) = return TBool

-- typeCheckExpr (EString _ string) = return TStr

-- typeCheckExpr (ELitInt _ integer) = return TInt

-- typeCheckExpr (Null _) = return TNull

-- typeCheckExpr (EVar _ lval) = typeCheckLVal lval

-- typeCheckExpr (ENewObj nr ident) = do
--     type_ <- typeCheckType $ Obj nr ident
--     return $ type_

-- typeCheckExpr (ECall nr ident exprs) = do
--     functions <- asks $ functions . env
--     case M.lookup ident functions of
--         Nothing -> throwError $ undeclaredIdentifierError nr ident 
--         Just fun -> do
--             types <- mapM typeCheckExpr exprs
--             compatibleTypes <- fmap and $ zipWithM isCompatible (args fun) types
--             unless (compatibleTypes && (length (args fun) == length types)) . throwError $ functionCallError nr ident
--             return . ret $ fun

-- typeCheckExpr (EMetCall nr expr ident exprs) = do
--     type_ <- typeCheckExpr expr
--     case type_ of
--         TObj classIdent -> do
--             functions <- fmap functions $ evalStateT (getClassEnv $ Just classIdent) S.empty
--             case M.lookup ident functions of
--                 Nothing -> throwError $ noMethodError nr classIdent ident
--                 Just fun -> do
--                     types <- mapM typeCheckExpr exprs
--                     compatibleTypes <- fmap and $ zipWithM isCompatible (args fun) types
--                     unless (compatibleTypes && (length (args fun) == length types)) . throwError $ methodCallError nr ident
--                     return . ret $ fun  
--         _ -> throwError $ classTypeError nr

-- typeCheckExpr (ENewArr nr type_ expr) = do
--     type_ <- typeCheckType type_
--     expType <- typeCheckExpr expr
--     unless (expType == TInt) . throwError $ arrayLengthTypeError nr
--     return $ TArr type_

-- typeCheckExpr (Neg nr expr) = do
--     expType <- typeCheckExpr expr
--     unless (expType == TInt) . throwError $ unaryOperatorTypeError nr expType "-"
--     return TInt

-- typeCheckExpr (Not nr expr) = do
--     expType <- typeCheckExpr expr
--     unless (expType == TBool) . throwError $ unaryOperatorTypeError nr expType "!"
--     return TBool

-- typeCheckExpr (EMul nr expr1 mulop expr2) = do
--     expType1 <- typeCheckExpr expr1
--     expType2 <- typeCheckExpr expr2
--     unless (expType1 == TInt && expType2 == TInt) . throwError $ binaryOperatorTypesError nr expType1 expType2 (showMulOp mulop)
--     return TInt

-- typeCheckExpr (EAdd nr expr1 addop expr2) = do
--     expType1 <- typeCheckExpr expr1
--     expType2 <- typeCheckExpr expr2
--     unless (expType1 == TInt && expType2 == TInt) . throwError $ binaryOperatorTypesError nr expType1 expType2 (showAddOp addop)
--     return TInt
  
-- typeCheckExpr (ERel nr expr1 relop expr2) = do
--     expType1 <- typeCheckExpr expr1
--     expType2 <- typeCheckExpr expr2
--     unless (compare relop expType1 expType2) . throwError $ binaryOperatorTypesError nr expType1 expType2 (showRelOp relop)
--     return TBool
--     where
--         compare _ TVoid _ = False
--         compare _ _ TVoid = False
--         compare _ TInt TInt = True
--         compare _ TBool TBool = True
--         compare (EQU _) a b = a == b
--         compare (NE _) a b = a == b
--         compare _ _ _ = False 

-- typeCheckExpr (EAnd nr expr1 expr2) = do
--     expType1 <- typeCheckExpr expr1
--     expType2 <- typeCheckExpr expr2
--     unless (expType1 == TBool && expType2 == TBool) . throwError $ binaryOperatorTypesError nr expType1 expType2 "&&"
--     return TBool

-- typeCheckExpr (EOr nr expr1 expr2) = do
--     expType1 <- typeCheckExpr expr1
--     expType2 <- typeCheckExpr expr2
--     unless (expType1 == TBool && expType2 == TBool) . throwError $ binaryOperatorTypesError nr expType1 expType2 "||"
--     return TBool

-- typeCheckType :: Type Loc -> ReaderT Context (ErrorT CompilerError IO) TType
-- typeCheckType (Void _) = return TVoid
-- typeCheckType (Int _) = return TInt 
-- typeCheckType (Str _) = return TStr 
-- typeCheckType (Bool _) = return TBool 
-- typeCheckType (Arr _ type_) = fmap TArr $ typeCheckType type_
-- typeCheckType (Obj nr ident) = do
--     classes <- asks $ classes . env
--     unless (M.member ident classes) . throwError $ unknownTypeName nr ident
--     return $ TObj ident

-- showAddOp :: AddOp a -> String
-- showAddOp (Plus _) = "+"
-- showAddOp (Minus _) = "-"

-- showMulOp :: MulOp a -> String
-- showMulOp (Times _) = "*"
-- showMulOp (Div _) = "/"
-- showMulOp (Mod _) = "%"

-- showRelOp :: RelOp a -> String
-- showRelOp (LTH _) = "<"
-- showRelOp (LE _) =  "<="
-- showRelOp (GTH _) = ">"
-- showRelOp (GE _) = ">="
-- showRelOp (EQU _) = "=="
-- showRelOp (NE _) = "!="

-- isCompatible :: TType -> TType -> ReaderT Context (ErrorT CompilerError IO) Bool
-- isCompatible (TObj _) TNull = return True
-- isCompatible (TObj ident1) (TObj ident2) = do
--     ancestors <- execStateT (getAncestors (Just ident2)) S.empty
--     return $ S.member ident1 ancestors
-- isCompatible type1 type2 = return $ type1 == type2


-- getAncestors :: (Maybe Ident) -> StateT (S.Set Ident) (ReaderT Context (ErrorT CompilerError IO)) ()
-- getAncestors Nothing = return ()
-- getAncestors (Just ident) = do
--     classes <- asks $ classes . env
--     case M.lookup ident classes of
--         Nothing -> throwError $ unknownClassName ident
--         (Just class_) -> do
--             idents <- get
--             when (S.member ident idents) . throwError $ inheritenceCycleError
--             modify $ S.insert ident
--             getAncestors $ parent class_
