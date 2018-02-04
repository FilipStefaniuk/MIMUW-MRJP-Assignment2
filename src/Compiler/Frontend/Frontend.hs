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
    _variableType :: ABS.Type,
    _variableAddress :: Register
}

data Function = Function {
    _functionType :: ABS.Type,
    _functionArguments :: [ABS.Type],
    _functionAddress :: FunctionAddr
}

data Class = Class {
    _classAddr :: UniqueId,
    _fieldsCount :: Integer,
    _className :: ABS.Ident,
    _classParent :: Maybe ABS.Ident,
    _classFields :: Map.Map ABS.Ident (ABS.Type, Integer),
    _classMethods :: Map.Map ABS.Ident (ABS.FunDef)
}

data GenMState = GenMState {
    _nextUniqueId :: !Integer,
    _stringCounter :: !Integer,
    _blockCounter :: !Integer,
    _localVariables :: Seq.Seq Alloc,
    _currentBlocks :: Map.Map Label Block,
    _currentBlockLabel :: Label,
    _arguments :: Seq.Seq Register,
    _strings :: Map.Map String Global,
    _program :: Program,
    _usedIdentifiers :: Set.Set String,
    _fields :: Seq.Seq Type
}

data GenMEnv = GenMEnv {
    _returnVar :: Maybe Variable,    
    _currentFunction :: Maybe (ABS.FunDef),
    _currentClass :: Maybe ABS.Ident,
    _variables :: Map.Map ABS.Ident Variable,
    _blockVariables :: Set.Set ABS.Ident,
    _functions :: Map.Map ABS.Ident Function,
    _classes :: Map.Map ABS.Ident Class
}

data GenMError = GenMError String
    deriving Show

instance Error GenMError where
    noMsg = GenMError "internal error"
    strMsg = GenMError

type GenM m = (MonadState GenMState m, MonadReader GenMEnv m, MonadError GenMError m, MonadIO m)

gen :: ABS.Program -> ErrorT GenMError IO Program
gen prog = _program <$> execStateT (runReaderT (transProgram prog) initGenMEnv) initGenMState
  where
    initGenMState :: GenMState
    initGenMState = GenMState {
        _nextUniqueId = 0,
        _localVariables = Seq.empty,
        _currentBlockLabel = Entry,
        _currentBlocks = Map.empty,
        _program = Program [] Seq.empty Seq.empty,
        _stringCounter = 0,
        _blockCounter = 0,
        _strings = Map.empty,
        _arguments = Seq.empty,
        _usedIdentifiers = Set.empty,
        _fields = Seq.empty
    }

    initGenMEnv :: GenMEnv
    initGenMEnv = GenMEnv{
        _returnVar = Nothing,
        _currentFunction = Nothing,
        _currentClass = Nothing,
        _variables = Map.empty,
        _blockVariables = Set.empty,
        _functions = getBuiltinFunctions,
        _classes = Map.empty
    }

getBuiltinFunctions :: Map.Map ABS.Ident Function
getBuiltinFunctions = Map.fromList [
    (ABS.Ident "printInt", Function ABS.Void [ABS.Int] (FunctionAddr (Ident "printInt") Void)),
    (ABS.Ident "printString", Function ABS.Void [ABS.Str] (FunctionAddr (Ident "printString") Void)),
    (ABS.Ident "error", Function ABS.Void [] (FunctionAddr (Ident "error") Void)),
    (ABS.Ident "readInt", Function ABS.Int [] (FunctionAddr (Ident "readInt") Size32)),
    (ABS.Ident "readString", Function ABS.Str [] (FunctionAddr (Ident "readString") (Ptr Size8))),
    (ABS.Ident "concat", Function ABS.Str [ABS.Str, ABS.Str] (FunctionAddr (Ident "concat") (Ptr Size8))),
    (ABS.Ident "malloc", Function ABS.Str [ABS.Int] (FunctionAddr (Ident "malloc") (Ptr Size8)))
    ]

transProgram :: GenM m => ABS.Program -> m ()
transProgram (ABS.Prog topdefs) = runContT (forM_ topdefs collectTopDef) (\_ -> forM_ topdefs transTopDef >> addStrings)
    where 
    collectTopDef :: GenM m => ABS.TopDef -> ContT () m ()
    collectTopDef (ABS.TopFunDef fundef) = collectFunDef fundef
    collectTopDef (ABS.ClassDef ident classitemdefs) = collectClassDef ident Nothing classitemdefs
    collectTopDef (ABS.ClassExtDef ident1 ident2 classitemdefs) =  collectClassDef ident1 (Just ident2) classitemdefs

    collectClassDef :: GenM m => ABS.Ident -> Maybe ABS.Ident -> [ABS.ClassItemDef] -> ContT () m ()
    collectClassDef ident@(ABS.Ident str) parent items = (lift . asks $ (Map.lookup ident) . _classes) >>= \case
        Just class_ -> lift . throwError $ GenMError "error"
        Nothing -> do
            firstField <- case parent of
                Nothing -> return 0
                Just ident -> return 1
            class_ <- lift $ execStateT (forM_ items collectClassItem) Class {
                _classAddr = UniqueId ("class." ++ str),
                _fieldsCount = firstField,
                _className = ident,
                _classParent = parent,
                _classFields = Map.empty,
                _classMethods = Map.empty
            }
            ContT $ \next -> local (\env -> env{_classes = Map.insert ident class_ $ _classes env}) $ next ()

    collectClassItem :: GenM m => ABS.ClassItemDef -> StateT Class m ()
    collectClassItem (ABS.AttrDef type_ ident) = (gets $ (Map.lookup ident) . _classFields) >>= \case
        Just _ -> throwError $ GenMError "error"
        Nothing -> modify $ \s -> s{_classFields = Map.insert ident (type_, _fieldsCount s) $ _classFields s, _fieldsCount = (_fieldsCount s) + 1}

    collectFunDef :: GenM m => ABS.FunDef -> ContT () m ()
    collectFunDef fun@(ABS.FunDef type_ ident@(ABS.Ident str) args block) = (lift . asks $ (Map.lookup ident) . _functions) >>= \case
        Just fun -> lift . throwError $ GenMError "error"
        Nothing -> ContT $ \next -> do
            ty <- transType type_
            local (\env -> env{_functions = Map.insert ident Function{
                _functionType = type_,
                _functionArguments = map (\(ABS.Ar type_ _) -> type_) args,
                _functionAddress = FunctionAddr (Ident str) ty
            } $ _functions env}) $ next ()

    addStrings :: GenM m => m()
    addStrings = do
        strings <- gets $ (map snd) . Map.toList . _strings
        modify (\s -> s{_program = (_program s){ _programStrings = strings}})

transTopDef :: GenM m => ABS.TopDef -> m ()
transTopDef (ABS.TopFunDef fundef) = transFunDef fundef
transTopDef (ABS.ClassDef ident items) = transClassDef ident Nothing items
transTopDef (ABS.ClassExtDef ident1 ident2 items) = transClassDef ident1 (Just ident2) items

transClassDef :: GenM m => ABS.Ident -> Maybe ABS.Ident -> [ABS.ClassItemDef] -> m ()
transClassDef ident parent items = do
    runStateT (checkInheritence parent) (Set.singleton ident)
    fieldInit <- case parent of
        Nothing -> return Seq.empty
        Just ident -> ((Seq.empty Seq.|>) . TypeClass) <$> (asks $ _classAddr . (Map.! ident) . _classes)   
    modify $ \s -> s {
        _fields = fieldInit
    }
    local (\env -> env{_currentClass = Just ident}) $ forM_ items transClassItem
    classAddr <-  asks $ _classAddr . (Map.! ident) . _classes
    fields <- gets _fields
    emitClassDef classAddr fields
  where
    checkInheritence :: GenM m => Maybe ABS.Ident -> StateT (Set.Set ABS.Ident) m () 
    checkInheritence Nothing = return ()
    checkInheritence (Just ident) = gets (Set.member ident) >>= \case
        True -> throwError $ GenMError "inheritance error"
        False -> do
            modify (Set.insert ident)
            class_ <- lift . asks $ _currentClass
            (lift . asks $ (Map.lookup ident) . _classes) >>= \case
                Nothing -> throwError $ GenMError "inheritance error"
                Just class_ -> checkInheritence (_classParent class_)
    
    transClassItem :: GenM m => ABS.ClassItemDef -> m ()
    transClassItem (ABS.AttrDef type_ ident) = do
        ty <- transType type_
        modify (\s -> s{_fields = ty Seq.<| _fields s})

    emitClassDef :: GenM m => UniqueId -> Seq.Seq Type -> m ()
    emitClassDef ident fields = modify $ \s -> s {
        _program = (_program s) {
            _classDefs = (ClassDef ident fields) Seq.<| (_classDefs . _program $ s)
        }
    }

transFunDef :: GenM m => ABS.FunDef -> m ()
transFunDef fun@(ABS.FunDef type_ (ABS.Ident str) args block) = do
    modify $ \s -> s {
        _nextUniqueId = 1,
        _blockCounter = 1,
        _localVariables = Seq.empty,
        _currentBlocks = Map.fromList [(Entry, Block Entry Seq.empty Seq.empty BlockEndNone)], 
        _currentBlockLabel = Entry,
        _arguments = Seq.empty,
        _usedIdentifiers = Set.empty
    }

    local (\env -> env{_currentFunction = Just fun}) $ runContT (forM_ args transArg >> transBlock block) return

    arguments <- gets _arguments
    vars <- gets _localVariables
    blocks <- gets _currentBlocks
    ty <- transType type_
    emitFunction $ FunctionDef {
        _functionAddr = FunctionAddr (Ident str) ty,
        _functionArgs = arguments,
        _localVars = vars,
        _blocks = blocks
    }
    where

    transArg :: GenM m => ABS.Arg -> ContT () m ()
    transArg (ABS.Ar type_ ident@(ABS.Ident str)) = asks (Set.member ident . _blockVariables) >>= \case
        True -> lift . throwError $ GenMError "error"
        False -> ContT $ \next -> do
            modify $ \s -> s{_usedIdentifiers = Set.insert str (_usedIdentifiers s)}
            ty <- transType type_
            reg' <- return $ Register (UniqueId str) ty
            reg <- flip Register (Ptr ty) <$> getNextUniqueId
            emitAlloc reg
            emitInstr $ InstrStore (Reg reg') reg
            modify $ \s -> s{_arguments = reg' Seq.<| _arguments s}
            local (\env -> env{
                _blockVariables = Set.insert ident $ _blockVariables env,
                _variables = Map.insert ident (Variable type_ reg) $ _variables env
            }) $ next ()

getVarName :: GenM m => ABS.Ident -> m UniqueId
getVarName (ABS.Ident str) = (gets $ (Set.member str) . _usedIdentifiers) >>= \case        
    False -> (modify $ \s -> s{_usedIdentifiers = Set.insert str (_usedIdentifiers s)}) >> (return $ UniqueId str)
    True -> do
        str <- getNextFreeName str 1
        modify $ \s -> s{_usedIdentifiers = Set.insert str (_usedIdentifiers s)}
        return $ UniqueId str
  where 
    getNextFreeName :: GenM m => String -> Integer -> m String
    getNextFreeName str int = (gets $ (Set.member (str ++ (show int))) . _usedIdentifiers) >>= \case
        False -> return $ str ++ (show int)
        True -> getNextFreeName str (int + 1)

transBlock :: GenM m => ABS.Block -> ContT () m ()
transBlock (ABS.Blk stmts) = lift $ do
    variables <- asks _variables
    blockVariables <- asks _blockVariables
    local (\env -> env{_blockVariables = Set.empty}) $ runContT (forM_ stmts transStmt) return

voidRun :: GenM m => (() -> m ()) -> m ()
voidRun next = do
    current <- get
    next ()
    modify (const current) 

transStmt :: GenM m => ABS.Stmt -> ContT () m ()
transStmt ABS.Empty = return ()
transStmt (ABS.BStmt block) = transBlock block
transStmt (ABS.SExp expr) = lift .void $ transExpr expr
transStmt (ABS.Decl type_ items) = forM_ items (transItem type_)
transStmt (ABS.Ass lval expr) = lift $ do
    (type_, reg) <- transLVal lval
    (type_, operand) <- transExpr expr
    -- TODO: check types
    emitInstr $ InstrStore operand reg 
    
transStmt (ABS.Incr lval) = transStmt $ ABS.Ass lval $ ABS.EAdd (ABS.EVar lval) ABS.Plus (ABS.ELitInt 1)
transStmt (ABS.Decr lval) = transStmt $ ABS.Ass lval $ ABS.EAdd (ABS.EVar lval) ABS.Minus (ABS.ELitInt 1)

transStmt (ABS.Ret expr) = do
    lift $ asks _currentFunction >>= \case
        Nothing -> throwError $ GenMError "error"
        Just (ABS.FunDef type_ _ _ _) -> do
            operand <- transExprRequireType type_ expr
            emitEnd $ BlockEndReturn operand
    ContT $ \next -> voidRun next

transStmt ABS.VRet = do
    lift $ emitEnd BlockEndReturnVoid
    ContT $ \next -> voidRun next
    
-- Sprawdzać poprawność skróconych wyrażeń
transStmt (ABS.Cond expr stmt) = lift $ do
    (type_, operand) <- transExpr expr
    case operand of
        ConstBool True -> local (\env -> env{_blockVariables = Set.empty}) $ runContT (transStmt stmt) return
        ConstBool False -> return ()
        _ -> do
            begin <- gets _currentBlockLabel
            trueBlock <- newBlock
            modify $ \s -> s{_currentBlockLabel = trueBlock}
            local (\env -> env{_blockVariables = Set.empty}) $ runContT (transStmt stmt) return
            end <- newBlock
            emitEnd $ BlockEndBranch end
            modify $ \s -> s{_currentBlockLabel = begin}
            emitEnd $ BlockEndBranchCond operand trueBlock end
            modify $ \s -> s{_currentBlockLabel = end}

transStmt (ABS.CondElse expr stmt1 stmt2) = do
    (type_, operand) <- lift $ transExpr expr
    case operand of
        ConstBool True -> lift $ local (\env -> env{_blockVariables = Set.empty}) $ runContT (transStmt stmt1) return 
        ConstBool False ->  lift $ local (\env -> env{_blockVariables = Set.empty}) $ runContT (transStmt stmt2) return 
        _ -> do
            begin <- lift $ gets _currentBlockLabel
            trueBlock <- lift $ newBlock
            lift $ modify $ \s -> s{_currentBlockLabel = trueBlock}
            lift $ local (\env -> env{_blockVariables = Set.empty}) $ runContT (transStmt stmt1) return
            isReturnTrueBlock <- lift $ isReturn
            falseBlock <- lift $ newBlock
            lift $ modify $ \s -> s{_currentBlockLabel = falseBlock}
            lift $ local (\env -> env{_blockVariables = Set.empty}) $ runContT (transStmt stmt2) return
            isReturnFalseBlock <- lift $ isReturn
            if not (isReturnTrueBlock && isReturnFalseBlock) then lift $ do
                end <- newBlock
                emitEnd $ BlockEndBranch end
                modify $ \s -> s{_currentBlockLabel = trueBlock}
                emitEnd $ BlockEndBranch end
                modify $ \s -> s{_currentBlockLabel = begin}
                emitEnd $ BlockEndBranchCond operand trueBlock falseBlock
                modify $ \s -> s{_currentBlockLabel = end}
            else do
                currentLabel <- gets _currentBlockLabel
                lift $ modify $ \s -> s{_currentBlockLabel = begin}
                lift $ emitEnd $ BlockEndBranchCond operand trueBlock falseBlock
                lift $ modify $ \s -> s{_currentBlockLabel = currentLabel}
                ContT $ \next -> voidRun $ \_ -> do
                    end <- newBlock 
                    modify $ \s -> s{_currentBlockLabel = end}
                    next ()

transStmt (ABS.While expr stmt) = lift $ do
    cond <- newBlock
    emitEnd $ BlockEndBranch cond
    modify $ \s -> s{_currentBlockLabel = cond}
    (type_, operand) <- transExpr expr
    case operand of
        ConstBool True -> do
            local (\env -> env{_blockVariables = Set.empty}) $ runContT (transStmt stmt) return
            emitEnd $ BlockEndBranch cond
            end <- newBlock
            modify $ \s -> s{_currentBlockLabel = end}
        _ -> do
            body <- newBlock
            modify $ \s -> s{_currentBlockLabel = body}
            local (\env -> env{_blockVariables = Set.empty}) $ runContT (transStmt stmt) return
            emitEnd $ BlockEndBranch cond
            end <- newBlock
            modify $ \s -> s{_currentBlockLabel = cond}
            emitEnd $ BlockEndBranchCond operand body end
            modify $ \s -> s{_currentBlockLabel = end}

--   For _ type_ ident expr stmt -> failure x

isReturn :: GenM m => m Bool
isReturn = do
    label <- gets _currentBlockLabel
    (gets $ _blockEnd . (Map.! label) . _currentBlocks) >>= \case
        BlockEndReturn _ -> return True
        BlockEndReturnVoid -> return True
        _ -> return False


transItem :: GenM m => ABS.Type -> ABS.Item -> ContT () m ()
transItem type_ (ABS.NoInit ident) = transItem type_ (ABS.Init ident (defaultInit type_))
  where
    defaultInit :: ABS.Type -> ABS.Expr
    defaultInit ABS.Int = ABS.ELitInt 0
    defaultInit ABS.Bool = ABS.ELitFalse
    defaultInit ABS.Str = ABS.EString ""
    defaultInit _ = ABS.ENull

transItem type_ (ABS.Init ident@(ABS.Ident str) expr) = do
    (lift $ asks (Set.member ident . _blockVariables)) >>= \case
        True -> lift . throwError $ GenMError "error"
        False -> ContT $ \next -> do
            uniqueId <- getVarName ident
            ty <- transType type_
            reg <- return $ Register uniqueId (Ptr ty)
            emitAlloc reg
            case expr of 
                ABS.ENull -> return ()
                expr -> do
                    (type1, operand) <- transExpr expr
                    emitInstr $ InstrStore operand reg
            local (\env -> env{
                _blockVariables = Set.insert ident $ _blockVariables env,
                _variables = Map.insert ident (Variable type_ reg) $ _variables env
            }) $ next ()



transType :: GenM m => ABS.Type -> m Type
transType ABS.Bool  = return Size1
transType ABS.Int  = return Size32
transType ABS.Str  = return $ Ptr Size8
transType (ABS.Arr type_) = Ptr <$> transType type_
transType ABS.Void = return Void
transType (ABS.Obj ident) = (asks $ (Map.lookup ident) . _classes) >>= \case
    Nothing -> throwError $ GenMError "error"
    Just class_ -> return . Ptr . TypeClass $ _classAddr class_

-- Null _ -> failure x

transExprRequireType :: GenM m => ABS.Type -> ABS.Expr -> m Operand
transExprRequireType type_ expr = do
    (exprType, operand ) <- transExpr expr
    unless (type_ == exprType) . throwError $ GenMError "error"
    return operand    

transExpr :: GenM m => ABS.Expr -> m (ABS.Type, Operand)
transExpr (ABS.ELitInt integer) = return (ABS.Int, ConstInt integer)
transExpr ABS.ELitTrue = return (ABS.Bool, ConstBool True)
transExpr ABS.ELitFalse = return (ABS.Bool, ConstBool False)
transExpr ABS.ENull = return (ABS.Null, Null)
transExpr (ABS.EVar lval) = do
    (type_, reg@(Register _ ty)) <- transLVal lval
    uniqueId <- getNextUniqueId
    reg' <- Register uniqueId <$> dereference ty
    emitInstr $ InstrLoad reg' reg 
    return (type_, Reg reg')
  where
    dereference :: GenM m => Type -> m Type
    dereference (Ptr type_) = return type_
    dereference _ = throwError $ GenMError "error"

transExpr (ABS.EString string) = do
    strAddr <- (gets $ (Map.lookup string) . _strings) >>= \case
        Just strAddr -> return strAddr
        Nothing -> do
            ident <- gets $ makeStringIdent . _stringCounter
            strAddr <- return $ GlobalString ident (Arr ((+1) . toInteger . length $ string) Size8) string
            modify (\s -> s{_strings = Map.insert string strAddr $ _strings s, _stringCounter = (_stringCounter s) + 1})
            return strAddr
    reg <- flip Register (Ptr $ Ptr Size8) <$> getNextUniqueId
    emitInstr $ InstrGetElementPtr reg (Glob strAddr) [(Size32, IdxConst 0), (Size32, IdxConst 0)]
    return (ABS.Str, Reg reg) 
  where
    makeStringIdent :: Integer -> Ident
    makeStringIdent int 
        | int == 0 = Ident ".str"
        | otherwise = Ident $ ".str." ++ (show int)

transExpr (ABS.ECall ident exprs) = (asks $ (Map.lookup ident) . _functions) >>= \case
    Nothing -> throwError $ GenMError "error"
    Just (Function type_ argTypes funAddr) -> do
        exprs <- mapM transExpr exprs
        case type_ of
            ABS.Void -> do
                emitInstr $ InstrVoidCall funAddr (map snd exprs)
                return (type_, Null)
            type_ -> do
                ty <- transType type_
                reg <- flip Register ty <$> getNextUniqueId
                emitInstr $ InstrCall reg funAddr (map snd exprs)
                return (type_, Reg reg)

--   EMetCall _ expr ident exprs -> failure x

transExpr (ABS.ENewObj ident) = do
    size <- computeClassSize (Just ident)
    clsAddr <- asks $ _classAddr . (Map.! ident) . _classes
    fun <- asks $ _functionAddress . (Map.! (ABS.Ident "malloc")) . _functions
    reg <- flip Register (Ptr Size8) <$> getNextUniqueId
    reg' <- flip Register (Ptr $ TypeClass clsAddr) <$> getNextUniqueId
    emitInstr $ InstrCall reg fun [ConstInt size]
    emitInstr $ InstrBitcast reg' (Reg reg)
    return (ABS.Obj ident, Reg reg')

  where
    computeClassSize :: GenM m => Maybe ABS.Ident -> m Integer
    computeClassSize Nothing = return 0
    computeClassSize (Just ident) = asks ((Map.lookup ident) . _classes) >>= \case
        Nothing -> throwError $ GenMError "error"
        Just class_ -> do
            size <- computeClassSize . _classParent $ class_
            return $ Map.foldr (accumFieldSize) size (_classFields class_)

    accumFieldSize :: (ABS.Type, Integer) -> Integer -> Integer
    accumFieldSize (type_, _) int = int + (getSize type_)
    
    getSize :: ABS.Type -> Integer
    getSize ABS.Int = 4
    getSize ABS.Bool = 1
    getSize ABS.Str = 8
    getSize (ABS.Arr _) = 8
    getSize (ABS.Obj _) = 8

transExpr (ABS.ENewArr type_ expr) = do
    operand <- transExprRequireType (ABS.Int) expr
    fun <- asks $ _functionAddress . (Map.! (ABS.Ident "malloc")) . _functions
    reg1 <- flip Register Size32 <$> getNextUniqueId
    reg2 <- flip Register (Ptr Size8) <$> getNextUniqueId
    ty <- transType type_
    reg3 <- flip Register (Ptr ty) <$> getNextUniqueId
    emitInstr $ InstrBinOp reg1 Times operand (ConstInt (getSize type_))
    emitInstr $ InstrCall reg2 fun [Reg reg1]
    emitInstr $ InstrBitcast reg3 (Reg reg2)
    return (type_, Reg reg3)
  where
    getSize :: ABS.Type -> Integer
    getSize ABS.Int = 4
    getSize ABS.Bool = 1
    getSize ABS.Str = 8
    getSize (ABS.Arr _) = 8

transExpr (ABS.Neg expr) = transExpr $ ABS.EMul expr ABS.Times (ABS.ELitInt (-1))
transExpr (ABS.Not expr) = transExpr $ ABS.ERel expr ABS.EQU (ABS.ELitFalse)

transExpr (ABS.EMul expr1 mulop expr2) = do
    operand1 <- transExprRequireType ( ABS.Int) expr1
    operand2 <- transExprRequireType ( ABS.Int) expr2
    case (operand1, mulop, operand2) of
        (ConstInt int1, ABS.Times, ConstInt int2) -> return ( ABS.Int, ConstInt $ int1 * int2)
        (ConstInt int1, ABS.Div, ConstInt int2) -> return ( ABS.Int, ConstInt $ int1 `div` int2)
        (ConstInt int1, ABS.Mod, ConstInt int2) -> return ( ABS.Int, ConstInt $ int1 `mod` int2)
        _ -> do
            reg <- flip Register Size32 <$> getNextUniqueId
            emitInstr $ InstrBinOp reg (transMulOp mulop) operand1 operand2
            return ( ABS.Int, Reg reg)
  where
    transMulOp :: ABS.MulOp -> Op
    transMulOp ABS.Times = Times
    transMulOp ABS.Div = Div
    transMulOp ABS.Mod = Mod

transExpr (ABS.EAdd expr1 addop expr2) = do
    (type1, operand1) <- transExpr expr1
    (type2, operand2) <- transExpr expr2
    case (type1, type2) of
        (ABS.Str, ABS.Str) -> do
            reg <- flip Register (Ptr Size8) <$> getNextUniqueId
            fun <- asks $ _functionAddress . (Map.! (ABS.Ident "concat")) . _functions
            emitInstr $ InstrCall reg fun [operand1, operand2]
            return (ABS.Str, Reg reg)
        (ABS.Int, ABS.Int) -> case (operand1, addop, operand2) of
            (ConstInt int1, ABS.Plus, ConstInt int2) -> return (type1, ConstInt $ int1 + int2)
            (ConstInt int1, ABS.Minus, ConstInt int2) -> return (type1, ConstInt $ int1 - int2)
            _ -> do
                reg <- flip Register Size32 <$> getNextUniqueId
                emitInstr $ InstrBinOp reg (transAddOp addop) operand1 operand2
                return (type1, Reg reg)
        _ -> throwError $ GenMError "error"
  where
    transAddOp :: ABS.AddOp -> Op
    transAddOp ABS.Plus = Plus
    transAddOp ABS.Minus = Minus

transExpr (ABS.ERel expr1 relop expr2) = do
    (type1, operand1) <- transExpr expr1
    (type2, operand2) <- transExpr expr2
    case (operand1, relop, operand2) of
        (ConstInt int1, relop, ConstInt int2) -> return (ABS.Bool, ConstBool $ computeRelop relop int1 int2)
        (ConstBool bool1, relop, ConstBool bool2) -> return (ABS.Bool, ConstBool $ computeRelop relop bool1 bool2)
        _ -> do
            reg <- flip Register Size1 <$> getNextUniqueId
            emitInstr $ InstrCmp reg (transRelOp relop) operand1 operand2
            return (ABS.Bool, Reg reg)
  where 
    computeRelop :: (Ord a, Eq a) => ABS.RelOp -> a -> a -> Bool
    computeRelop ABS.LTH lhs rhs = lhs < rhs
    computeRelop ABS.LE lhs rhs = lhs <= rhs
    computeRelop ABS.GTH lhs rhs = lhs > rhs
    computeRelop ABS.GE lhs rhs = lhs >= rhs
    computeRelop ABS.EQU lhs rhs = lhs == rhs
    computeRelop ABS.NE lhs rhs = lhs /= rhs

    transRelOp :: ABS.RelOp -> Cond
    transRelOp ABS.LTH = LTH
    transRelOp ABS.LE = LE
    transRelOp ABS.GTH = GTH
    transRelOp ABS.GE = GE
    transRelOp ABS.EQU = EQU
    transRelOp ABS.NE = NE

-- TODO Sprawdzać typy skróconych wyrażeń !!!
transExpr (ABS.EAnd expr1 expr2) = do
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

transExpr (ABS.EOr expr1 expr2) = do
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


transLVal :: GenM m => ABS.LVal -> m (ABS.Type, Register)
transLVal (ABS.LVar ident) = asks (Map.lookup ident . _variables) >>= \case
    Nothing -> throwError $ GenMError "error"
    Just var -> return (_variableType var, _variableAddress var)

transLVal (ABS.LArr expr1 expr2) = do
    operand2 <- transExprRequireType ABS.Int expr2
    (type_, operand1) <- transExpr expr1
    idx <- case operand2 of
        ConstInt int -> return $ IdxConst int
        (Reg (Register id _)) -> return $ IdxAddr id
    case type_ of
        ABS.Arr type_ -> do
            ty <- transType type_
            reg <- flip Register (Ptr ty) <$> getNextUniqueId
            emitInstr $ InstrGetElementPtr reg operand1 [(Size32, idx)]
            return (type_ , reg)
        _ -> throwError $ GenMError "error"

transLVal (ABS.LAttr expr ident) = do
    (type_, operand) <- transExpr expr
    case type_ of 
        ABS.Obj classIdent -> do
            class_ <- getClassByField (Just classIdent) ident
            operand <- if _className class_ == classIdent then
                    return operand
                else do
                    reg <- flip Register (Ptr . TypeClass . _classAddr $ class_) <$> getNextUniqueId
                    emitInstr $ InstrBitcast reg operand
                    return $ Reg reg
            (type_, nr) <- return . (Map.! ident) . _classFields $ class_
            ty <- transType type_
            reg <- flip Register (Ptr ty) <$> getNextUniqueId
            emitInstr $ InstrGetElementPtr reg operand [(Size32, IdxConst 0), (Size32, IdxConst nr)]
            return (type_, reg)
  where
    getClassByField :: GenM m => Maybe ABS.Ident -> ABS.Ident -> m Class
    getClassByField Nothing _ = throwError $ GenMError "error"
    getClassByField (Just classIdent) fieldIdent = asks ((Map.lookup classIdent) . _classes) >>= \case
        Nothing -> throwError $ GenMError "error"
        Just class_ -> if (Map.member fieldIdent) . _classFields $ class_ then return class_ 
            else getClassByField (_classParent class_) fieldIdent 

--   LSelf _ -> failure x



-- transClassItemDef :: Show a => ClassItemDef a -> Result
-- transClassItemDef x = case x of
--     AttrDef _ type_ ident -> failure x
--     MethodDef _ fundef -> failure x

newBlock :: GenM m => m Label
newBlock = do
    uniqueId <- getNextUniqueId
    nr <- gets _blockCounter
    label <- return $ Label nr uniqueId
    modify $ \s -> s{_blockCounter = nr + 1, _currentBlocks = Map.insert label (Block label Seq.empty Seq.empty BlockEndNone) (_currentBlocks s)}
    return label

getNextUniqueId :: GenM m => m UniqueId
getNextUniqueId = do
    uniqueId <- gets _nextUniqueId
    modify $ \s -> s{_nextUniqueId = uniqueId + 1}
    return $ UniqueId ((show uniqueId))    

emitFunction :: GenM m => FunctionDef -> m ()
emitFunction funcDef = do
    modify (\s -> s{_program = (_program s) {
        _functionDefs = funcDef Seq.<| _functionDefs (_program s)
    }})

emitAlloc :: GenM m => Register -> m ()
emitAlloc reg = modify $ \s -> s{_localVariables = (Alloc reg) Seq.<| _localVariables s}

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
emitEnd end = isReachable >>= flip when (do
    label <- gets _currentBlockLabel
    block <- gets $ (Map.! label) . _currentBlocks
    modify (\s -> s{_currentBlocks = Map.insert label (block{_blockEnd=end}) $ _currentBlocks s}))
  where
    isReachable :: GenM m => m Bool
    isReachable = do
        label <- gets _currentBlockLabel
        (gets $ _blockEnd . (Map.! label) . _currentBlocks) >>= \case
            BlockEndNone -> return True
            _ -> return False
