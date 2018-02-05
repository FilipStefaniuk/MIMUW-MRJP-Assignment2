{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Frontend.Frontend where
  
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Monad.Extra
import Data.Foldable
import Data.Maybe
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
    _variableAddress :: LocalIdent
}

data Block = Block {
    _blockNumber :: Integer,
    _blockPhi :: Seq.Seq Phi,
    _blockBody :: Seq.Seq Instruction,
    _blockEnd :: BlockEnd
}

data Function = Function {
    _functionName :: ABS.Ident,
    _functionType :: ABS.Type,
    _functionArguments :: [ABS.Type],
    _functionAddress :: GlobalIdent
}

data Class = Class {
    _classAddr :: LocalIdent,
    _fieldsCount :: Integer,
    _className :: ABS.Ident,
    _classParent :: Maybe ABS.Ident,
    _classFields :: Map.Map ABS.Ident (ABS.Type, Integer),
    _classMethods :: Map.Map ABS.Ident Function
}

data GenMState = GenMState {
    _nextUniqueId :: !Integer,
    _stringCounter :: !Integer,
    _blockCounter :: !Integer,
    _localVariables :: Seq.Seq Alloc,
    _blocks :: Map.Map Integer Block,
    _currentBlock :: Integer,
    _arguments :: Seq.Seq (Type, LocalIdent),
    _usedIdentifiers :: Set.Set String,
    _fields :: Seq.Seq Type,
    
    _stringsDefs :: Map.Map String StringDef,
    _classDefs :: Seq.Seq ClassDef,
    _functionDefs :: Seq.Seq FunctionDef
}

data GenMEnv = GenMEnv {
    
    _returnVar :: Maybe Variable,    
    _currentFunction :: Maybe ABS.Ident,
    _currentClass :: Maybe ABS.Ident,

    _blockVariables :: Set.Set ABS.Ident,
    _variables :: Map.Map ABS.Ident Variable,
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
gen prog = evalStateT (runReaderT (transProgram prog) initGenMEnv) initGenMState
  where
    initGenMState :: GenMState
    initGenMState = GenMState {
        _nextUniqueId = 0,
        _localVariables = Seq.empty,
        _currentBlock = 0,
        _blocks = Map.empty,
        _stringCounter = 0,
        _blockCounter = 0,
        _arguments = Seq.empty,
        _usedIdentifiers = Set.empty,
        _fields = Seq.empty,
        
        _stringsDefs = Map.empty,
        _classDefs = Seq.empty,
        _functionDefs = Seq.empty
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
    (ABS.Ident "printInt", Function (ABS.Ident "printInt") ABS.Void [ABS.Int] (GlobalIdent "printInt")),
    (ABS.Ident "printString", Function (ABS.Ident "printString") ABS.Void [ABS.Str] (GlobalIdent "printString")),
    (ABS.Ident "error", Function (ABS.Ident "error") ABS.Void [] (GlobalIdent "error")),
    (ABS.Ident "readInt", Function (ABS.Ident "readInt") ABS.Int [] (GlobalIdent "readInt")),
    (ABS.Ident "readString", Function (ABS.Ident "readString") ABS.Str [] (GlobalIdent "readString")),
    (ABS.Ident "concat", Function (ABS.Ident "concat") ABS.Str [ABS.Str, ABS.Str] (GlobalIdent "concat")),
    (ABS.Ident "malloc", Function (ABS.Ident "malloc") ABS.Str [ABS.Int] (GlobalIdent "malloc"))
    ]

transProgram :: GenM m => ABS.Program -> m Program
transProgram (ABS.Prog topdefs) = do
    runContT (forM_ topdefs collectTopDef) $ \_ -> forM_ topdefs transTopDef
    classDefs <- gets $ toList . Seq.reverse . _classDefs
    stringDefs <- gets $ Map.elems . _stringsDefs
    functionDefs <- gets $ toList . Seq.reverse . _functionDefs
    return (Program classDefs stringDefs functionDefs)
  
  where
    collectTopDef :: GenM m => ABS.TopDef -> ContT () m ()
    collectTopDef (ABS.ClassDef ident items) = collectClassDef ident Nothing >> forM_ items (collectClassItem ident)
    collectTopDef (ABS.ClassExtDef ident1 ident2 items) = collectClassDef ident1 (Just ident2) >> forM_ items (collectClassItem ident1)
    collectTopDef (ABS.TopFunDef fun@(ABS.FunDef _ ident@(ABS.Ident str) _ _)) = (lift . asks $ (Map.lookup ident) . _functions) >>= \case
        Just _ -> lift . throwError $ GenMError "Error: multiple function declaration"
        Nothing -> ContT $ \next -> local (\env -> env{
            _functions = Map.insert ident (getFunction str fun) $ _functions env
        }) $ next ()

    collectClassDef :: GenM m => ABS.Ident -> Maybe ABS.Ident ->  ContT () m ()
    collectClassDef ident@(ABS.Ident str) parent = (lift . asks $ (Map.lookup ident) . _classes) >>= \case
        Just class_ -> lift . throwError $ GenMError "ERROR: multiple class definition"
        Nothing -> ContT $ \next ->  local (\env -> env{_classes = Map.insert ident Class{
            _classAddr = LocalIdent $ "class." ++ str,
            _fieldsCount = if isJust $ parent then 1 else 0,
            _className = ident,
            _classParent = parent,
            _classFields = Map.empty,
            _classMethods = Map.empty
        } $ _classes env}) $ next ()

    collectClassItem :: GenM m => ABS.Ident -> ABS.ClassItemDef -> ContT () m ()
    collectClassItem classIdent (ABS.AttrDef type_ ident) = (lift . asks $ (Map.lookup classIdent) . _classes) >>= \case
        Nothing -> lift . throwError $ GenMError "ERROR: internal error"
        Just class_ -> case Map.lookup ident $ _classFields class_ of
            Just _ -> lift . throwError $ GenMError "ERROR: multiple class field declaration"
            Nothing -> ContT $ \next -> local (\env -> env{_classes = Map.insert classIdent class_{
                _classFields = Map.insert ident (type_, _fieldsCount class_) $ _classFields class_,
                _fieldsCount = (_fieldsCount class_) + 1
            } $ _classes env}) $ next ()

    collectClassItem classIdent@(ABS.Ident str1) (ABS.MethodDef fun@(ABS.FunDef _ ident@(ABS.Ident str2) _ _)) = 
        (lift . asks $ (Map.lookup classIdent) . _classes) >>= \case
        Nothing -> lift . throwError $ GenMError "ERROR: internal error"
        Just class_ -> case Map.lookup ident $ _classMethods class_ of
            Just _ -> lift . throwError $ GenMError "ERROR: multiple method declaration"
            Nothing -> ContT $ \next -> local (\env -> env{_classes = Map.insert classIdent class_{
                _classMethods = Map.insert ident (getFunction (str1 ++ "." ++ str2) fun) $ _classMethods class_
            } $ _classes env}) $ next ()
    
    getFunction :: String -> ABS.FunDef -> Function
    getFunction name (ABS.FunDef type_ ident args block) = Function {
        _functionName = ident,
        _functionType = type_,
        _functionArguments = map (\(ABS.Ar type_ _) -> type_) args,
        _functionAddress = GlobalIdent name
    }
    
transTopDef :: GenM m => ABS.TopDef -> m ()
transTopDef (ABS.ClassDef ident items) = transClassDef ident Nothing items
transTopDef (ABS.ClassExtDef ident1 ident2 items) = transClassDef ident1 (Just ident2) items
transTopDef (ABS.TopFunDef fun@(ABS.FunDef _ ident _ _)) = do
    funAddr <- asks $ _functionAddress . (Map.! ident) . _functions
    transFunDef funAddr fun

transClassDef :: GenM m => ABS.Ident -> Maybe ABS.Ident -> [ABS.ClassItemDef] -> m ()
transClassDef ident parent items = do
    checkInheritence parent (Set.singleton ident)
    fields <- case parent of
        Nothing -> return Seq.empty
        Just parent -> fmap ((Seq.empty Seq.|>) . TypeClass) (asks $ _classAddr . (Map.! parent) . _classes)   
    modify $ \s -> s {_fields = fields}
    local (\env -> env{_currentClass = Just ident}) $ forM_ items transClassItem
    classAddr <- asks $ _classAddr . (Map.! ident) . _classes
    fields <- gets $ toList . Seq.reverse . _fields
    modify $ \s -> s {_classDefs = (ClassDef classAddr fields) Seq.<| _classDefs s}
  where

    transClassItem :: GenM m => ABS.ClassItemDef -> m ()
    transClassItem (ABS.AttrDef type_ ident) = do
        ty <- transType type_
        modify (\s -> s{_fields = ty Seq.<| _fields s})

    transClassItem (ABS.MethodDef (ABS.FunDef type_ ident args block)) = do
        Just classIdent <- asks _currentClass
        class_ <- asks $ (Map.! classIdent) . _classes
        checkOverride (_classParent class_) ((Map.! ident) . _classMethods $ class_)
        funAddr <- return . _functionAddress . (Map.! ident) . _classMethods $ class_
        transFunDef funAddr (ABS.FunDef type_ ident ((ABS.Ar (ABS.Obj classIdent) (ABS.Ident "this")):args) block)

    checkInheritence :: GenM m => Maybe ABS.Ident -> Set.Set ABS.Ident -> m () 
    checkInheritence Nothing  _ = return ()
    checkInheritence (Just ident) descendants = (asks $ (Map.lookup ident) . _classes) >>= \case
        Nothing -> throwError $ GenMError "ERROR: unknown class identifier"
        Just class_ -> if Set.member ident descendants then
            throwError $ GenMError "ERROR: inheritance cycle"
            else checkInheritence (_classParent class_) (Set.insert ident descendants)
    
    checkOverride :: GenM m => Maybe ABS.Ident -> Function -> m ()
    checkOverride Nothing _ = return ()
    checkOverride (Just ident) fun@(Function _ retType argTypes _) = do
        class_ <- asks $ (Map.! ident) . _classes
        case (Map.lookup (_functionName fun)) . _classMethods $ class_ of
            Nothing -> checkOverride (_classParent class_) fun
            Just (Function _ retType2 argTypes2 _) -> unless (retType == retType2 && argTypes == argTypes2) . throwError $ GenMError "override error"

transType :: GenM m => ABS.Type -> m Type
transType ABS.Bool  = return Size8
transType ABS.Int  = return Size32
transType ABS.Str  = return $ Ptr Size8
transType (ABS.Arr type_) = return $ Ptr Size8
transType ABS.Void = return Void
transType (ABS.Obj ident) = (asks $ (Map.lookup ident) . _classes) >>= \case
    Nothing -> throwError $ GenMError "ERROR: Unknown type"
    Just class_ -> return . Ptr . TypeClass . _classAddr $ class_

transFunDef :: GenM m => GlobalIdent -> ABS.FunDef -> m ()
transFunDef funAddr fun@(ABS.FunDef type_ ident args (ABS.Blk stmts)) = do
    modify $ \s -> s {
        _nextUniqueId = 1,
        _blockCounter = 1,
        _localVariables = Seq.empty,
        _blocks = Map.fromList [(0, Block 0 Seq.empty Seq.empty BlockEndNone)], 
        _currentBlock = 0,
        _arguments = Seq.empty,
        _usedIdentifiers = Set.empty
    }

    local (\env -> env{_currentFunction = Just ident}) $ runContT (forM_ args transArg >> forM_ stmts transStmt) (const setReturn) 

    ty <- transType type_
    arguments <- gets $ toList . Seq.reverse . _arguments
    vars <- gets $ toList . Seq.reverse . _localVariables
    blocks <- gets $ (map toCodeBlock) . Map.elems . _blocks
    modify $ \s -> s {_functionDefs = (FunctionDef ty funAddr arguments vars blocks) Seq.<| (_functionDefs s) }

  where
    toCodeBlock :: Block -> CodeBlock
    toCodeBlock block = let 
        ident = LocalIdent . show . _blockNumber $ block
        phi = toList . Seq.reverse . _blockPhi $ block
        instructions = toList . Seq.reverse . _blockBody $ block 
        end = _blockEnd block in
            CodeBlock ident phi instructions end

    setReturn :: GenM m => m ()
    setReturn = do
        (Just ident) <- asks _currentFunction
        (asks $ _functionType . (Map.! ident) . _functions) >>= \case
            ABS.Void -> emitEnd $ BlockEndReturnVoid
            _ -> ifM (isReturn) (return ()) (throwError $ GenMError "ERROR: missing return statement")

transArg :: GenM m => ABS.Arg -> ContT () m ()
transArg (ABS.Ar type_ ident@(ABS.Ident str)) = asks (Set.member ident . _blockVariables) >>= \case
    True -> lift . throwError $ GenMError "ERROR: multiple arguments declaration"
    False -> ContT $ \next -> do
        modify $ \s -> s{_usedIdentifiers = Set.insert str (_usedIdentifiers s)}
        when (type_ == ABS.Void) . throwError $ GenMError "ERROR: void type argument"
        ty <- transType type_
        varName <- return $ LocalIdent str
        res <- getNextLocalIdent
        emitInstr $ InstrStore ty (Loc varName) (Ptr ty) res 
        
        modify $ \s -> s{
            _localVariables = (Alloc res (Ptr ty)) Seq.<| (_localVariables s),
            _arguments = (ty, varName) Seq.<| (_arguments s)
        }
        
        local (\env -> env{
            _blockVariables = Set.insert ident $ _blockVariables env,
            _variables = Map.insert ident (Variable type_ res) $ _variables env
        }) $ next ()

transBlock :: GenM m => ABS.Block -> ContT () m ()
transBlock (ABS.Blk stmts) = do
    lift . local (\env -> env{_blockVariables = Set.empty}) $ runContT (forM_ stmts transStmt) return
    ifM (lift isReturn) (ContT $ \next -> return ()) (return ())

transStmt :: GenM m => ABS.Stmt -> ContT () m ()
transStmt ABS.Empty = return ()
transStmt (ABS.BStmt block) = transBlock block
-- transStmt (ABS.SExp expr) = lift .void $ transExpr expr
-- transStmt (ABS.Decl type_ items) = forM_ items (transItem type_)
-- transStmt (ABS.Ass lval expr) = lift $ do
--     (type1, reg) <- transLVal lval
--     (type2, operand) <- transExpr expr
--     -- TODO: check types
--     ty1 <- transType type1
--     ty2 <- transType type2
--     emitInstr $ InstrStore ty2 operand ty1 reg 
    
-- transStmt (ABS.Incr lval) = transStmt $ ABS.Ass lval $ ABS.EAdd (ABS.EVar lval) ABS.Plus (ABS.ELitInt 1)
-- transStmt (ABS.Decr lval) = transStmt $ ABS.Ass lval $ ABS.EAdd (ABS.EVar lval) ABS.Minus (ABS.ELitInt 1)

-- transStmt (ABS.Ret expr) = do
--     lift $ asks _currentFunction >>= \case
--         Nothing -> throwError $ GenMError "error"
--         Just (ABS.FunDef type_ _ _ _) -> do
--             operand <- transExprRequireType type_ expr
--             emitEnd $ BlockEndReturn operand
--     ContT $ \next -> voidRun next

-- transStmt ABS.VRet = do
--     lift $ emitEnd BlockEndReturnVoid
--     ContT $ \next -> voidRun next
    
-- -- Sprawdzać poprawność skróconych wyrażeń
-- transStmt (ABS.Cond expr stmt) = lift $ do
--     (type_, operand) <- transExpr expr
--     case operand of
--         ConstBool True -> local (\env -> env{_blockVariables = Set.empty}) $ runContT (transStmt stmt) return
--         ConstBool False -> return ()
--         _ -> do
--             begin <- gets _currentBlockLabel
--             trueBlock <- newBlock
--             modify $ \s -> s{_currentBlockLabel = trueBlock}
--             local (\env -> env{_blockVariables = Set.empty}) $ runContT (transStmt stmt) return
--             end <- newBlock
--             emitEnd $ BlockEndBranch end
--             modify $ \s -> s{_currentBlockLabel = begin}
--             emitEnd $ BlockEndBranchCond operand trueBlock end
--             modify $ \s -> s{_currentBlockLabel = end}

-- transStmt (ABS.CondElse expr stmt1 stmt2) = do
--     (type_, operand) <- lift $ transExpr expr
--     case operand of
--         ConstBool True -> lift $ local (\env -> env{_blockVariables = Set.empty}) $ runContT (transStmt stmt1) return 
--         ConstBool False ->  lift $ local (\env -> env{_blockVariables = Set.empty}) $ runContT (transStmt stmt2) return 
--         _ -> do
--             begin <- lift $ gets _currentBlockLabel
--             trueBlock <- lift $ newBlock
--             lift $ modify $ \s -> s{_currentBlockLabel = trueBlock}
--             lift $ local (\env -> env{_blockVariables = Set.empty}) $ runContT (transStmt stmt1) return
--             isReturnTrueBlock <- lift $ isReturn
--             falseBlock <- lift $ newBlock
--             lift $ modify $ \s -> s{_currentBlockLabel = falseBlock}
--             lift $ local (\env -> env{_blockVariables = Set.empty}) $ runContT (transStmt stmt2) return
--             isReturnFalseBlock <- lift $ isReturn
--             if not (isReturnTrueBlock && isReturnFalseBlock) then lift $ do
--                 end <- newBlock
--                 emitEnd $ BlockEndBranch end
--                 modify $ \s -> s{_currentBlockLabel = trueBlock}
--                 emitEnd $ BlockEndBranch end
--                 modify $ \s -> s{_currentBlockLabel = begin}
--                 emitEnd $ BlockEndBranchCond operand trueBlock falseBlock
--                 modify $ \s -> s{_currentBlockLabel = end}
--             else do
--                 currentLabel <- gets _currentBlockLabel
--                 lift $ modify $ \s -> s{_currentBlockLabel = begin}
--                 lift $ emitEnd $ BlockEndBranchCond operand trueBlock falseBlock
--                 lift $ modify $ \s -> s{_currentBlockLabel = currentLabel}
--                 ContT $ \next -> voidRun $ \_ -> do
--                     end <- newBlock 
--                     modify $ \s -> s{_currentBlockLabel = end}
--                     next ()

-- transStmt (ABS.While expr stmt) = lift $ do
--     cond <- newBlock
--     emitEnd $ BlockEndBranch cond
--     modify $ \s -> s{_currentBlockLabel = cond}
--     (type_, operand) <- transExpr expr
--     case operand of
--         ConstBool True -> do
--             local (\env -> env{_blockVariables = Set.empty}) $ runContT (transStmt stmt) return
--             emitEnd $ BlockEndBranch cond
--             end <- newBlock
--             modify $ \s -> s{_currentBlockLabel = end}
--         _ -> do
--             body <- newBlock
--             modify $ \s -> s{_currentBlockLabel = body}
--             local (\env -> env{_blockVariables = Set.empty}) $ runContT (transStmt stmt) return
--             emitEnd $ BlockEndBranch cond
--             end <- newBlock
--             modify $ \s -> s{_currentBlockLabel = cond}
--             emitEnd $ BlockEndBranchCond operand body end
--             modify $ \s -> s{_currentBlockLabel = end}

-- transStmt (ABS.For type_ ident expr stmt) = transBlock (ABS.Blk [
--     ABS.Decl type_ [(ABS.NoInit ident)],
--     ABS.Decl ABS.Int [(ABS.NoInit (ABS.Ident ".t"))],
--     (ABS.While (ABS.ERel (ABS.EVar (ABS.LVar (ABS.Ident ".t"))) ABS.LTH (ABS.EVar (ABS.LAttr expr (ABS.Ident "length"))))
--     (ABS.BStmt (ABS.Blk [
--         ABS.Ass (ABS.LVar ident) (ABS.EVar (ABS.LArr expr (ABS.EVar (ABS.LVar (ABS.Ident ".t"))))),
--         stmt,
--         ABS.Incr (ABS.LVar (ABS.Ident ".t"))
--     ])))])

-- isReturn :: GenM m => m Bool
-- isReturn = do
--     label <- gets _currentBlockLabel
--     (gets $ _blockEnd . (Map.! label) . _currentBlocks) >>= \case
--         BlockEndReturn _ -> return True
--         BlockEndReturnVoid -> return True
--         _ -> return False


-- transItem :: GenM m => ABS.Type -> ABS.Item -> ContT () m ()
-- transItem type_ (ABS.NoInit ident) = transItem type_ (ABS.Init ident (defaultInit type_))
--   where
--     defaultInit :: ABS.Type -> ABS.Expr
--     defaultInit ABS.Int = ABS.ELitInt 0
--     defaultInit ABS.Bool = ABS.ELitFalse
--     defaultInit ABS.Str = ABS.EString ""
--     defaultInit _ = ABS.ENull

-- transItem type_ (ABS.Init ident@(ABS.Ident str) expr) = do
--     (lift $ asks (Set.member ident . _blockVariables)) >>= \case
--         True -> lift . throwError $ GenMError "error"
--         False -> ContT $ \next -> do
--             uniqueId <- getVarName ident
--             ty <- transType type_
--             emitAlloc $ Alloc uniqueId ty
--             case expr of 
--                 ABS.ENull -> return ()
--                 expr -> do
--                     (type1, operand) <- transExpr expr
--                     ty2 <- transType type1
--                     emitInstr $ InstrStore ty operand ty2 uniqueId
--             local (\env -> env{
--                 _blockVariables = Set.insert ident $ _blockVariables env,
--                 _variables = Map.insert ident (Variable type_ uniqueId) $ _variables env
--             }) $ next ()

-- Null _ -> failure x

-- getClassByMethod :: GenM m => Maybe ABS.Ident -> ABS.Ident -> m (Maybe ABS.Ident)
-- getClassByMethod Nothing _ = return Nothing
-- getClassByMethod (Just classIdent) methodIdent = asks ((Map.lookup classIdent) . _classes) >>= \case
--     Nothing -> return Nothing
--     Just class_ -> if (Map.member methodIdent) . _classMethods $ class_ then return . Just $ _className class_ 
--         else getClassByMethod (_classParent class_) methodIdent 

-- transExprRequireType :: GenM m => ABS.Type -> ABS.Expr -> m Operand
-- transExprRequireType type_ expr = do
--     (exprType, operand ) <- transExpr expr
--     unless (type_ == exprType) . throwError $ GenMError "error"
--     return operand    

-- transExpr :: GenM m => ABS.Expr -> m (ABS.Type, Operand)
-- transExpr (ABS.ELitInt integer) = return (ABS.Int, ConstInt integer)
-- transExpr ABS.ELitTrue = return (ABS.Bool, ConstBool True)
-- transExpr ABS.ELitFalse = return (ABS.Bool, ConstBool False)
-- transExpr ABS.ENull = return (ABS.Null, Null)
-- transExpr (ABS.EVar lval) = do
--     (type_, localIdent) <- transLVal lval
--     ty <- transType type_
--     uniqueId <- getNextUniqueId
--     emitInstr $ InstrLoad uniqueId (Ptr ty) ty localIdent
--     return (type_, uniqueId)
--   where
--     dereference :: GenM m => Type -> m Type
--     dereference (Ptr type_) = return type_
--     dereference _ = throwError $ GenMError "error"

-- transExpr (ABS.EString string) = do
--     strAddr <- (gets $ (Map.lookup string) . _strings) >>= \case
--         Just strAddr -> return strAddr
--         Nothing -> do
--             ident <- gets $ makeStringIdent . _stringCounter
--             strAddr <- return $ GlobalString ident (Arr ((+1) . toInteger . length $ string) Size8) string
--             modify (\s -> s{_strings = Map.insert string strAddr $ _strings s, _stringCounter = (_stringCounter s) + 1})
--             return strAddr
--     reg <- flip Register (Ptr $ Ptr Size8) <$> getNextUniqueId
--     emitInstr $ InstrGetElementPtr reg (Glob strAddr) [(Size32, IdxConst 0), (Size32, IdxConst 0)]
--     return (ABS.Str, Reg reg) 
--   where
--     makeStringIdent :: Integer -> GlobalIdent
--     makeStringIdent int 
--         | int == 0 = GlobalIdent ".str"
--         | otherwise = GlobalIdent $ ".str." ++ (show int)

-- transExpr call@(ABS.ECall ident exprs) = asks (_currentClass) >>= \case 
--     Nothing -> transFunctionCall call
--     justclassIdent -> getClassByMethod justclassIdent ident >>= \case
--         Nothing ->  transFunctionCall call
--         Just classIdent -> transExpr (ABS.EMetCall (ABS.EVar ABS.LSelf) ident exprs)
--   where
--     transFunctionCall :: GenM m => ABS.Expr -> m (ABS.Type, Operand)
--     transFunctionCall (ABS.ECall ident exprs) = (asks $ (Map.lookup ident) . _functions) >>= \case
--         Nothing -> throwError $ GenMError "error"
--         Just (Function _ type_ argTypes funAddr) -> do
--             exprs <- mapM transExpr exprs
--             case type_ of
--                 ABS.Void -> do
--                     emitInstr $ InstrVoidCall funAddr (map snd exprs)
--                     return (type_, Null)
--                 type_ -> do
--                     ty <- transType type_
--                     reg <- flip Register ty <$> getNextUniqueId
--                     emitInstr $ InstrCall reg funAddr (map snd exprs)
--                     return (type_, Reg reg)

-- transExpr (ABS.EMetCall expr ident exprs) = do
--     (type_, operand) <- transExpr expr
--     exprs <- mapM transExpr exprs
--     case type_ of
--         ABS.Obj classIdent -> do 
--             classIdent' <- getClassByMethod (Just classIdent) ident >>= \case
--                 Just classIdent -> return classIdent
--                 Nothing -> throwError $ GenMError "error"
--             class_ <- asks $ (Map.! classIdent') . _classes 
--             operand <- if classIdent' == classIdent then
--                     return operand
--                 else do
--                     reg <- flip Register (Ptr . TypeClass . _classAddr $ class_) <$> getNextUniqueId
--                     emitInstr $ InstrBitcast reg operand
--                     return $ Reg reg
--             let (Function _ retType argTypes funAddr) = (Map.! ident) . _classMethods $ class_ in
--                 case retType of
--                     ABS.Void -> do
--                         emitInstr $ InstrVoidCall funAddr (operand:(map snd exprs))
--                         return (retType, Null)
--                     type_ -> do
--                         ty <- transType type_
--                         reg <- flip Register ty <$> getNextUniqueId
--                         emitInstr $ InstrCall reg funAddr (operand:(map snd exprs))
--                         return (type_, Reg reg)


-- transExpr (ABS.ENewObj ident) = do
--     size <- computeClassSize (Just ident)
--     clsAddr <- asks $ _classAddr . (Map.! ident) . _classes
--     fun <- asks $ _functionAddress . (Map.! (ABS.Ident "malloc")) . _functions
--     reg <- flip Register (Ptr Size8) <$> getNextUniqueId
--     reg' <- flip Register (Ptr $ TypeClass clsAddr) <$> getNextUniqueId
--     emitInstr $ InstrCall reg fun [ConstInt size]
--     emitInstr $ InstrBitcast reg' (Reg reg)
--     return (ABS.Obj ident, Reg reg')

--   where
--     computeClassSize :: GenM m => Maybe ABS.Ident -> m Integer
--     computeClassSize Nothing = return 0
--     computeClassSize (Just ident) = asks ((Map.lookup ident) . _classes) >>= \case
--         Nothing -> throwError $ GenMError "error"
--         Just class_ -> do
--             size <- computeClassSize . _classParent $ class_
--             return $ Map.foldr (accumFieldSize) size (_classFields class_)

--     accumFieldSize :: (ABS.Type, Integer) -> Integer -> Integer
--     accumFieldSize (type_, _) int = int + (getSize type_)
    
--     getSize :: ABS.Type -> Integer
--     getSize ABS.Int = 4
--     getSize ABS.Bool = 1
--     getSize ABS.Str = 1
--     getSize (ABS.Arr _) = 1
--     getSize (ABS.Obj _) = 1

-- transExpr (ABS.ENewArr type_ expr) = do
--     operand <- transExprRequireType (ABS.Int) expr
--     fun <- asks $ _functionAddress . (Map.! (ABS.Ident "malloc")) . _functions
--     reg1 <- flip Register Size32 <$> getNextUniqueId
--     reg2 <- flip Register Size32 <$> getNextUniqueId
--     reg3 <- flip Register (Ptr Size8) <$> getNextUniqueId
--     reg4 <- flip Register (Ptr Size32) <$> getNextUniqueId
    
--     emitInstr $ InstrBinOp reg1 Times operand (ConstInt (getSize type_))
--     emitInstr $ InstrBinOp reg2 Plus (Reg reg1) (ConstInt (getSize ABS.Int))
--     emitInstr $ InstrCall reg3 fun [Reg reg2]
--     emitInstr $ InstrBitcast reg4 (Reg reg3)
--     emitInstr $ InstrStore operand reg4
--     return (ABS.Arr type_, Reg reg3)
--   where
--     getSize :: ABS.Type -> Integer
--     getSize ABS.Int = 4
--     getSize ABS.Bool = 1
--     getSize ABS.Str = 1
--     getSize (ABS.Arr _) = 1

-- transExpr (ABS.Neg expr) = transExpr $ ABS.EMul expr ABS.Times (ABS.ELitInt (-1))
-- transExpr (ABS.Not expr) = transExpr $ ABS.ERel expr ABS.EQU (ABS.ELitFalse)

-- transExpr (ABS.EMul expr1 mulop expr2) = do
--     operand1 <- transExprRequireType ( ABS.Int) expr1
--     operand2 <- transExprRequireType ( ABS.Int) expr2
--     case (operand1, mulop, operand2) of
--         (ConstInt int1, ABS.Times, ConstInt int2) -> return ( ABS.Int, ConstInt $ int1 * int2)
--         (ConstInt int1, ABS.Div, ConstInt int2) -> return ( ABS.Int, ConstInt $ int1 `div` int2)
--         (ConstInt int1, ABS.Mod, ConstInt int2) -> return ( ABS.Int, ConstInt $ int1 `mod` int2)
--         _ -> do
--             reg <- flip Register Size32 <$> getNextUniqueId
--             emitInstr $ InstrBinOp reg (transMulOp mulop) operand1 operand2
--             return ( ABS.Int, Reg reg)
--   where
--     transMulOp :: ABS.MulOp -> Op
--     transMulOp ABS.Times = Times
--     transMulOp ABS.Div = Div
--     transMulOp ABS.Mod = Mod

-- transExpr (ABS.EAdd expr1 addop expr2) = do
--     (type1, operand1) <- transExpr expr1
--     (type2, operand2) <- transExpr expr2
--     case (type1, type2) of
--         (ABS.Str, ABS.Str) -> do
--             reg <- flip Register (Ptr Size8) <$> getNextUniqueId
--             fun <- asks $ _functionAddress . (Map.! (ABS.Ident "concat")) . _functions
--             emitInstr $ InstrCall reg fun [operand1, operand2]
--             return (ABS.Str, Reg reg)
--         (ABS.Int, ABS.Int) -> case (operand1, addop, operand2) of
--             (ConstInt int1, ABS.Plus, ConstInt int2) -> return (type1, ConstInt $ int1 + int2)
--             (ConstInt int1, ABS.Minus, ConstInt int2) -> return (type1, ConstInt $ int1 - int2)
--             _ -> do
--                 reg <- flip Register Size32 <$> getNextUniqueId
--                 emitInstr $ InstrBinOp reg (transAddOp addop) operand1 operand2
--                 return (type1, Reg reg)
--         _ -> throwError $ GenMError "error"
--   where
--     transAddOp :: ABS.AddOp -> Op
--     transAddOp ABS.Plus = Plus
--     transAddOp ABS.Minus = Minus

-- transExpr (ABS.ERel expr1 relop expr2) = do
--     (type1, operand1) <- transExpr expr1
--     (type2, operand2) <- transExpr expr2
--     case (operand1, relop, operand2) of
--         (ConstInt int1, relop, ConstInt int2) -> return (ABS.Bool, ConstBool $ computeRelop relop int1 int2)
--         (ConstBool bool1, relop, ConstBool bool2) -> return (ABS.Bool, ConstBool $ computeRelop relop bool1 bool2)
--         _ -> do
--             reg <- flip Register Size1 <$> getNextUniqueId
--             emitInstr $ InstrCmp reg (transRelOp relop) operand1 operand2
--             return (ABS.Bool, Reg reg)
--   where 
--     computeRelop :: (Ord a, Eq a) => ABS.RelOp -> a -> a -> Bool
--     computeRelop ABS.LTH lhs rhs = lhs < rhs
--     computeRelop ABS.LE lhs rhs = lhs <= rhs
--     computeRelop ABS.GTH lhs rhs = lhs > rhs
--     computeRelop ABS.GE lhs rhs = lhs >= rhs
--     computeRelop ABS.EQU lhs rhs = lhs == rhs
--     computeRelop ABS.NE lhs rhs = lhs /= rhs

--     transRelOp :: ABS.RelOp -> Cond
--     transRelOp ABS.LTH = LTH
--     transRelOp ABS.LE = LE
--     transRelOp ABS.GTH = GTH
--     transRelOp ABS.GE = GE
--     transRelOp ABS.EQU = EQU
--     transRelOp ABS.NE = NE

-- TODO Sprawdzać typy skróconych wyrażeń !!!
-- transExpr (ABS.EAnd expr1 expr2) = do
--     (type1, operand1) <- transExpr expr1
--     case operand1 of
--         ConstBool False -> return (type1, operand1)
--         ConstBool True -> do
--             (type2, operand2) <- transExpr expr2
--             return (type2, operand2)
--         _ -> do
--             first <- gets _currentBlockLabel
--             mid <- newBlock
--             modify $ \s -> s{_currentBlockLabel = mid}
--             (type2, operand2) <- transExpr expr2
--             end <- newBlock
--             emitEnd $ BlockEndBranch end
--             modify $ \s -> s{_currentBlockLabel = first}
--             emitEnd $ BlockEndBranchCond operand1 mid end
--             modify $ \s -> s{_currentBlockLabel = end}
--             reg <- flip Register Size1 <$> getNextUniqueId
--             emitPhi $ Phi reg [PhiBranch first (ConstBool False), PhiBranch mid operand2]
--             return (type1, Reg reg)

-- transExpr (ABS.EOr expr1 expr2) = do
--     (type1, operand1) <- transExpr expr1
--     case operand1 of
--         ConstBool True -> return (type1, operand1)
--         ConstBool False -> do
--             (type2, operand2) <- transExpr expr2
--             return (type2, operand2)
--         _ -> do
--             first <- gets _currentBlockLabel
--             mid <- newBlock
--             modify $ \s -> s{_currentBlockLabel = mid}
--             (type2, operand2) <- transExpr expr2
--             end <- newBlock
--             emitEnd $ BlockEndBranch end
--             modify $ \s -> s{_currentBlockLabel = first}
--             emitEnd $ BlockEndBranchCond operand1 end mid
--             modify $ \s -> s{_currentBlockLabel = end}
--             reg <- flip Register Size1 <$> getNextUniqueId
--             emitPhi $ Phi reg [PhiBranch first (ConstBool True), PhiBranch mid operand2]
--             return (type1, Reg reg)


-- transLVal :: GenM m => ABS.LVal -> m (ABS.Type, LocalIdent)
-- transLVal (ABS.LVar ident) = asks (Map.lookup ident . _variables) >>= \case
--     Nothing -> asks _currentClass >>= \case
--         Nothing -> throwError $ GenMError "error"
--         Just _ -> transLVal $ (ABS.LAttr (ABS.EVar ABS.LSelf) ident)
--     Just var -> return (_variableType var, _variableAddress var)

-- transLVal (ABS.LArr expr1 expr2) = do
--     operand2 <- transExprRequireType ABS.Int expr2
--     (type_, operand1) <- transExpr expr1
--     idx <- case operand2 of
--         ConstInt int -> return $ IdxConst int
--         (Reg (Register id _)) -> return $ IdxAddr id
--     case type_ of
--         ABS.Arr type_ -> do
--             ty <- transType type_
--             reg <- flip Register (Ptr Size8) <$> getNextUniqueId
--             reg2 <- flip Register (Ptr ty) <$> getNextUniqueId
--             reg3 <- flip Register (Ptr ty) <$> getNextUniqueId
--             emitInstr $ InstrGetElementPtr reg operand1 [(Size32, IdxConst 4)]
--             emitInstr $ InstrBitcast reg2 (Reg reg)
--             emitInstr $ InstrGetElementPtr reg3 (Reg reg2) [(Size32, idx)]
--             return (type_ , reg3)
--         _ -> throwError $ GenMError "error"

-- transLVal (ABS.LAttr expr ident) = do
--     (type_, operand) <- transExpr expr
--     case type_ of
--         ABS.Arr _ -> do
--             unless (ident == (ABS.Ident "length")) $ throwError $ GenMError "error"
--             reg <- flip Register (Ptr Size32) <$> getNextUniqueId
--             emitInstr $ InstrBitcast reg operand
--             return (ABS.Int, reg)
--         ABS.Obj classIdent -> do
--             class_ <- getClassByField (Just classIdent) ident
--             operand <- if _className class_ == classIdent then
--                     return operand
--                 else do
--                     reg <- flip Register (Ptr . TypeClass . _classAddr $ class_) <$> getNextUniqueId
--                     emitInstr $ InstrBitcast reg operand
--                     return $ Reg reg
--             (type_, nr) <- return . (Map.! ident) . _classFields $ class_
--             ty <- transType type_
--             reg <- flip Register (Ptr ty) <$> getNextUniqueId
--             emitInstr $ InstrGetElementPtr reg operand [(Size32, IdxConst 0), (Size32, IdxConst nr)]
--             return (type_, reg)
--   where
--     getClassByField :: GenM m => Maybe ABS.Ident -> ABS.Ident -> m Class
--     getClassByField Nothing _ = throwError $ GenMError "error"
--     getClassByField (Just classIdent) fieldIdent = asks ((Map.lookup classIdent) . _classes) >>= \case
--         Nothing -> throwError $ GenMError "error"
--         Just class_ -> if (Map.member fieldIdent) . _classFields $ class_ then return class_ 
--             else getClassByField (_classParent class_) fieldIdent 

-- transLVal ABS.LSelf = (asks _currentClass) >>= \case
--     Nothing -> throwError $ GenMError "error"
--     Just ident -> do
--         ty <- transType (ABS.Obj ident)
--         return (ABS.Obj ident, LocalIdent "1")

-- newBlock :: GenM m => m LocalIdent
-- newBlock = do
--     uniqueId <- getNextUniqueId
--     modify $ \s -> s{_currentBlocks = Map.insert uniqueId (Block uniqueId Seq.empty Seq.empty BlockEndNone) (_currentBlocks s)}
--     return uniqueId

getNextId :: GenM m => m Integer
getNextId = do
    uniqueId <- gets _nextUniqueId
    modify $ \s -> s{_nextUniqueId = uniqueId + 1}
    return $ uniqueId
    
getNextLocalIdent :: GenM m => m LocalIdent
getNextLocalIdent = fmap (LocalIdent . show) getNextId

emitInstr :: GenM m => Instruction -> m ()
emitInstr instr = do
    nr <- gets _currentBlock
    block <- gets $ (Map.! nr) . _blocks
    modify (\s -> s{_blocks = Map.insert nr (block{_blockBody = instr Seq.<| _blockBody block}) $ _blocks s})

-- emitPhi :: GenM m => Phi -> m ()
-- emitPhi phi = do
--     label <- gets _currentBlockLabel
--     block <- gets $ (Map.! label) . _currentBlocks
--     modify (\s -> s{_currentBlocks = Map.insert label (block{_blockPhi= phi Seq.<| _blockPhi block}) $ _currentBlocks s})

emitEnd :: GenM m => BlockEnd -> m ()
emitEnd end = do
    nr <- gets _currentBlock
    block <- gets $ (Map.! nr) . _blocks
    modify (\s -> s{_blocks = Map.insert nr (block{_blockEnd=end}) $ _blocks s})

isReturn :: GenM m => m Bool
isReturn  = do
    nr <- gets _currentBlock
    (gets $ _blockEnd . (Map.! nr) . _blocks) >>= \case
        BlockEndReturn _ _ -> return True
        BlockEndReturnVoid -> return True
        _ -> return False

-- getVarName :: GenM m => ABS.Ident -> m LocalIdent
-- getVarName (ABS.Ident str) = (gets $ (Set.member str) . _usedIdentifiers) >>= \case        
--     False -> (modify $ \s -> s{_usedIdentifiers = Set.insert str (_usedIdentifiers s)}) >> (return $ LocalIdent str)
--     True -> do
--         str <- getNextFreeName str 1
--         modify $ \s -> s{_usedIdentifiers = Set.insert str (_usedIdentifiers s)}
--         return $ LocalIdent str
--   where 
--     getNextFreeName :: GenM m => String -> Integer -> m String
--     getNextFreeName str int = (gets $ (Set.member (str ++ (show int))) . _usedIdentifiers) >>= \case
--         False -> return $ str ++ (show int)
--         True -> getNextFreeName str (int + 1)

-- voidRun :: GenM m => (() -> m ()) -> m ()
-- voidRun next = do
--     current <- get
--     next ()
--     modify (const current) 
