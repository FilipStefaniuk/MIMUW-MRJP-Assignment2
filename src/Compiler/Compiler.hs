{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Compiler where
  
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import qualified Control.Monad.Cont as Cont
import Data.Foldable
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified AbsLatte as ABS
import LexLatte
import ParLatte
import ErrM

import LLVM

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
    _currentFunction :: Maybe Function,
    _currentClass :: Maybe Class,

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
    (ABS.Ident "malloc", Function (ABS.Ident "malloc") ABS.Str [ABS.Int] (GlobalIdent "malloc")),
    (ABS.Ident "llvm.memset", Function(ABS.Ident "llvm.memset") ABS.Void [] (GlobalIdent "llvm.memset.p0i8.i32"))
    ]

transProgram :: GenM m => ABS.Program -> m Program
transProgram (ABS.Prog topdefs) = do
    Cont.runContT (mapM_ collectTopDef topdefs) $ \_ -> mapM_ transTopDef topdefs >> checkMain
    classDefs <- gets $ toList . Seq.reverse . _classDefs
    stringDefs <- gets $ Map.elems . _stringsDefs
    functionDefs <- gets $ toList . Seq.reverse . _functionDefs
    return (Program classDefs stringDefs functionDefs)
  
  where
    checkMain :: GenM m => m ()
    checkMain = (asks $ (Map.lookup (ABS.Ident "main") . _functions)) >>= \case
        Nothing -> throwError $ GenMError "ERROR: missing main function"
        Just (Function _ type_ types _) -> unless (type_ == ABS.Int && null types) . throwError $ GenMError "ERROR: wrong type of main function"


    collectTopDef :: GenM m => ABS.TopDef -> Cont.ContT () m ()
    collectTopDef (ABS.ClassDef ident items) = collectClassDef ident Nothing >> mapM_ (collectClassItem ident) items
    collectTopDef (ABS.ClassExtDef ident1 ident2 items) = collectClassDef ident1 (Just ident2) >> mapM_ (collectClassItem ident1) items
    collectTopDef (ABS.TopFunDef fun@(ABS.FunDef _ ident@(ABS.Ident str) _ _)) = (lift . asks $ (Map.lookup ident) . _functions) >>= \case
        Just _ -> lift . throwError $ GenMError "ERROR: multiple function declaration"
        Nothing -> Cont.ContT $ \next -> local (\env -> env{
            _functions = Map.insert ident (getFunction str fun) $ _functions env
        }) $ next ()

    collectClassDef :: GenM m => ABS.Ident -> Maybe ABS.Ident ->  Cont.ContT () m ()
    collectClassDef ident@(ABS.Ident str) parent = (lift . asks $ (Map.lookup ident) . _classes) >>= \case
        Just class_ -> lift . throwError $ GenMError "ERROR: multiple class definition"
        Nothing -> Cont.ContT $ \next ->  local (\env -> env{_classes = Map.insert ident Class{
            _classAddr = LocalIdent $ "class." ++ str,
            _fieldsCount = if isJust $ parent then 1 else 0,
            _className = ident,
            _classParent = parent,
            _classFields = Map.empty,
            _classMethods = Map.empty
        } $ _classes env}) $ next ()

    collectClassItem :: GenM m => ABS.Ident -> ABS.ClassItemDef -> Cont.ContT () m ()
    collectClassItem classIdent (ABS.AttrDef type_ ident) = (lift . asks $ (Map.lookup classIdent) . _classes) >>= \case
        Nothing -> lift . throwError $ GenMError "ERROR: internal error"
        Just class_ -> case Map.lookup ident $ _classFields class_ of
            Just _ -> lift . throwError $ GenMError "ERROR: multiple class field declaration"
            Nothing -> Cont.ContT $ \next -> local (\env -> env{_classes = Map.insert classIdent class_{
                _classFields = Map.insert ident (type_, _fieldsCount class_) $ _classFields class_,
                _fieldsCount = (_fieldsCount class_) + 1
            } $ _classes env}) $ next ()

    collectClassItem classIdent@(ABS.Ident str1) (ABS.MethodDef fun@(ABS.FunDef _ ident@(ABS.Ident str2) _ _)) = 
        (lift . asks $ (Map.lookup classIdent) . _classes) >>= \case
        Nothing -> lift . throwError $ GenMError "ERROR: internal error"
        Just class_ -> case Map.lookup ident $ _classMethods class_ of
            Just _ -> lift . throwError $ GenMError "ERROR: multiple method declaration"
            Nothing -> let method = getFunction (str1 ++ "." ++ str2) fun in
                Cont.ContT $ \next -> local (\env -> env{_classes = Map.insert classIdent class_{
                _classMethods = Map.insert ident (method{
                    _functionArguments = (ABS.Obj . _className $ class_):(_functionArguments method)
                }) $ _classMethods class_} $ _classes env}) $ next ()
    
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
transTopDef (ABS.TopFunDef funDef@(ABS.FunDef _ ident _ _)) = do
    fun <- asks $ (Map.! ident) . _functions
    transFunDef fun funDef

transClassDef :: GenM m => ABS.Ident -> Maybe ABS.Ident -> [ABS.ClassItemDef] -> m ()
transClassDef ident parent items = do
    checkInheritence parent (Set.singleton ident)
    fields <- case parent of
        Nothing -> return Seq.empty
        Just parent -> fmap ((Seq.empty Seq.|>) . TypeClass) (asks $ _classAddr . (Map.! parent) . _classes)   
    modify $ \s -> s {_fields = fields}
    class_ <- asks $ (Map.! ident) . _classes
    local (\env -> env{_currentClass = Just class_}) $ mapM_ transClassItem items
    classAddr <- asks $ _classAddr . (Map.! ident) . _classes
    fields <- gets $ toList . Seq.reverse . _fields
    modify $ \s -> s {_classDefs = (ClassDef classAddr fields) Seq.<| _classDefs s}
  where

    transClassItem :: GenM m => ABS.ClassItemDef -> m ()
    transClassItem (ABS.AttrDef type_ ident) = do
        ty <- transType type_
        modify (\s -> s{_fields = ty Seq.<| _fields s})

    transClassItem (ABS.MethodDef (ABS.FunDef type_ ident args block)) = do
        Just class_ <- asks _currentClass
        checkOverride (_classParent class_) ((Map.! ident) . _classMethods $ class_)
        fun <- return . (Map.! ident) . _classMethods $ class_
        transFunDef fun (ABS.FunDef type_ ident ((ABS.Ar (ABS.Obj . _className $ class_) (ABS.Ident "self")):args) block)

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

transFunDef :: GenM m => Function -> ABS.FunDef -> m ()
transFunDef fun (ABS.FunDef type_ ident args (ABS.Blk stmts)) = do
    modify $ \s -> s {
        _nextUniqueId = 1,
        _blockCounter = 1,
        _localVariables = Seq.empty,
        _blocks = Map.fromList [(0, Block 0 Seq.empty Seq.empty BlockEndNone)], 
        _currentBlock = 0,
        _arguments = Seq.empty,
        _usedIdentifiers = Set.empty
    }

    local (\env -> env{_currentFunction = Just fun}) $ Cont.runContT (mapM_ transArg args >> mapM_ transStmt stmts) (const setReturn) 

    ty <- transType type_
    funAddr <- return . _functionAddress $ fun
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
        (Just fun) <- asks $ _currentFunction
        case _functionType fun of 
            ABS.Void -> emitEnd $ BlockEndReturnVoid
            _ -> (isReturn) >>= \case 
                True -> (return ()) 
                False -> (throwError $ GenMError "ERROR: missing return statement")

transArg :: GenM m => ABS.Arg -> Cont.ContT () m ()
transArg (ABS.Ar type_ ident@(ABS.Ident str)) = asks (Set.member ident . _blockVariables) >>= \case
    True -> lift . throwError $ GenMError "ERROR: multiple arguments declaration"
    False -> Cont.ContT $ \next -> do
        modify $ \s -> s{_usedIdentifiers = Set.insert str (_usedIdentifiers s)}
        when (type_ == ABS.Void) . throwError $ GenMError "ERROR: void type argument"
        ty <- transType type_
        varName <- return $ LocalIdent str
        res <- getNextLocalIdent
        emitInstr $ InstrStore ty (Loc varName) (Ptr ty) res 
        
        modify $ \s -> s{
            _localVariables = (Alloc res ty) Seq.<| (_localVariables s),
            _arguments = (ty, varName) Seq.<| (_arguments s)
        }
        
        local (\env -> env{
            _blockVariables = Set.insert ident $ _blockVariables env,
            _variables = Map.insert ident (Variable type_ res) $ _variables env
        }) $ next ()

transBlock :: GenM m => ABS.Block -> Cont.ContT () m ()
transBlock (ABS.Blk stmts) = do
    lift . local (\env -> env{_blockVariables = Set.empty}) $ Cont.runContT (mapM_ transStmt stmts) return
    (lift isReturn) >>= \case 
        True -> (Cont.ContT $ \next -> return ()) 
        False -> (return ())

transStmt :: GenM m => ABS.Stmt -> Cont.ContT () m ()
transStmt ABS.Empty = return ()

transStmt (ABS.BStmt block) = transBlock block

transStmt (ABS.SExp expr) = lift . void $ transExpr expr

transStmt (ABS.Decl type_ items) = mapM_ (transItem type_) items

transStmt (ABS.Ass lval expr) = lift $ do
    (type1, addr) <- transLVal lval
    (type2, operand) <- transExprCompatibleType type1 expr >>= uncurry zextBool
    ty1 <- transType type1
    ty2 <- transType type2
    emitInstr $ InstrStore ty2 operand (Ptr ty1) addr 
    
transStmt (ABS.Incr lval) = transStmt . (ABS.Ass lval) $ ABS.EAdd (ABS.EVar lval) ABS.Plus (ABS.ELitInt 1)

transStmt (ABS.Decr lval) = transStmt . (ABS.Ass lval) $ ABS.EAdd (ABS.EVar lval) ABS.Minus (ABS.ELitInt 1)

transStmt (ABS.Ret expr) = do
    (Just fun) <- lift . asks $ _currentFunction
    (type_, op) <- lift $ transExprCompatibleType (_functionType fun) expr >>= uncurry zextBool
    ty <- lift $ transType type_
    lift . emitEnd $ BlockEndReturn ty op
    Cont.ContT $ \next -> return ()

transStmt ABS.VRet = do
    (Just fun) <- lift . asks $ _currentFunction
    lift . unless (_functionType fun == ABS.Void) . throwError $ GenMError "ERROR: void return in non void function"
    lift . emitEnd $ BlockEndReturnVoid
    Cont.ContT $ \next -> return ()
    
transStmt (ABS.Cond expr stmt) = do
    operand <- lift $ transExprRequireType ABS.Bool expr
    case operand of
        ConstBool True -> transBlock (ABS.Blk [stmt])
        ConstBool False -> return ()
        _ -> lift $ do
            begin <- gets _currentBlock

            ifBlock <- newBlock
            modify $ \s -> s{_currentBlock = ifBlock}
            Cont.runContT (transBlock (ABS.Blk [stmt])) return
            
            end <- newBlock
            emitEnd $ BlockEndBranch (LocalIdent . show $ end)
            (inBlock begin) . emitEnd $ BlockEndBranchCond Size1 operand (LocalIdent . show $ ifBlock) (LocalIdent . show $ end)
            modify $ \s -> s{_currentBlock = end}
            
transStmt (ABS.CondElse expr stmt1 stmt2) = do
    operand <- lift $ transExprRequireType ABS.Bool expr
    case operand of
        ConstBool True -> transBlock (ABS.Blk [stmt1]) 
        ConstBool False ->  transBlock (ABS.Blk [stmt2]) 
        _ -> do
            begin <- lift $ gets _currentBlock
            
            trueBlock <- lift $ newBlock
            lift . modify $ \s -> s{_currentBlock = trueBlock}
            lift $ Cont.runContT (transBlock (ABS.Blk [stmt1])) return
            trueBlockRet <- lift $ isReturn

            falseBlock <- lift $ newBlock
            lift . modify $ \s -> s{_currentBlock = falseBlock}
            lift $ Cont.runContT (transBlock (ABS.Blk [stmt2])) return
            falseBlockRet <- lift $ isReturn

            lift . (inBlock begin) . emitEnd $ BlockEndBranchCond Size1 operand (LocalIdent . show $ trueBlock) (LocalIdent . show $ falseBlock)
            
            if trueBlockRet && falseBlockRet then Cont.ContT $ \next -> return () else lift $ do
                end <- newBlock
                emitEnd $ BlockEndBranch (LocalIdent . show $ end)
                (inBlock trueBlock) . emitEnd $ BlockEndBranch (LocalIdent . show $ end)
                modify $ \s -> s{_currentBlock = end}
                
transStmt (ABS.While expr stmt) = lift $ do
    cond <- newBlock
    emitEnd $ BlockEndBranch (LocalIdent . show $ cond)
    modify $ \s -> s{_currentBlock = cond}
    operand <- transExprRequireType ABS.Bool expr
    case operand of
        ConstBool True -> do
            Cont.runContT (transBlock (ABS.Blk [stmt])) return
            emitEnd $ BlockEndBranch (LocalIdent . show $ cond)
            end <- newBlock
            modify $ \s -> s{_currentBlock = end}
        _ -> do
            body <- newBlock
            modify $ \s -> s{_currentBlock = body}
            Cont.runContT (transBlock (ABS.Blk [stmt])) return
            emitEnd $ BlockEndBranch (LocalIdent . show $ cond)
            end <- newBlock
            (inBlock cond) . emitEnd $ BlockEndBranchCond Size1 operand (LocalIdent . show $ body) (LocalIdent . show $ end)
            modify $ \s -> s{_currentBlock = end}

transStmt (ABS.For type_ ident expr stmt) = transBlock (ABS.Blk [
    ABS.Decl type_ [(ABS.NoInit ident)],
    ABS.Decl ABS.Int [(ABS.NoInit (ABS.Ident ".t"))],
    (ABS.While (ABS.ERel (ABS.EVar (ABS.LVar (ABS.Ident ".t"))) ABS.LTH (ABS.EVar (ABS.LAttr expr (ABS.Ident "length"))))
    (ABS.BStmt (ABS.Blk [
        ABS.Ass (ABS.LVar ident) (ABS.EVar (ABS.LArr expr (ABS.EVar (ABS.LVar (ABS.Ident ".t"))))),
        stmt,
        ABS.Incr (ABS.LVar (ABS.Ident ".t"))
    ])))])

transItem :: GenM m => ABS.Type -> ABS.Item -> Cont.ContT () m ()
transItem type_ (ABS.NoInit ident) = transItem type_ (ABS.Init ident (defaultInit type_))
  where
    defaultInit :: ABS.Type -> ABS.Expr
    defaultInit ABS.Int = ABS.ELitInt 0
    defaultInit ABS.Bool = ABS.ELitFalse
    defaultInit _ = ABS.ENull

transItem type1 (ABS.Init ident@(ABS.Ident str) expr) = 
    (lift $ asks (Set.member ident . _blockVariables)) >>= \case 
    True -> (lift . throwError $ GenMError "Error: Multiple variable declaration")
    False -> Cont.ContT $ \next -> do
            varName <- getVarName ident
            (type2, operand) <- transExprCompatibleType type1 expr >>= uncurry zextBool
            ty1 <- transType type1
            ty2 <- transType type2
            emitInstr $ InstrStore ty2 operand (Ptr ty1) varName

            modify $ \s -> s{_localVariables = (Alloc varName ty1) Seq.<| (_localVariables s)}

            local (\env -> env{
                _blockVariables = Set.insert ident $ _blockVariables env,
                _variables = Map.insert ident (Variable type1 varName) $ _variables env
            }) $ next ()

  where
    getVarName :: GenM m => ABS.Ident -> m LocalIdent
    getVarName (ABS.Ident str) = do
        (gets $ (Set.member str) . _usedIdentifiers) >>= \case
            True -> (getNextFreeName str 1) 
            False -> (return str)
        modify $ \s -> s{_usedIdentifiers = Set.insert str (_usedIdentifiers s)}
        return $ LocalIdent str

    getNextFreeName :: GenM m => String -> Integer -> m String
    getNextFreeName str int = (gets $ (Set.member (str ++ (show int))) . _usedIdentifiers) >>= \case
        True -> getNextFreeName str (int + 1)
        False -> return $ str ++ (show int)

transExpr :: GenM m => ABS.Expr -> m (ABS.Type, Operand)
transExpr (ABS.ELitInt integer) = return (ABS.Int, ConstInt integer)

transExpr ABS.ELitTrue = return (ABS.Bool, ConstBool True)

transExpr ABS.ELitFalse = return (ABS.Bool, ConstBool False)

transExpr ABS.ENull = return (ABS.Null, Null)

transExpr (ABS.EVar lval) = do
    (type_, addr) <- transLVal lval
    ty <- transType type_
    res <- getNextLocalIdent
    emitInstr $ InstrLoad res ty (Ptr ty) addr
    if type_ /= ABS.Bool then return (type_, Loc res) else do
        res2 <- getNextLocalIdent
        emitInstr $ InstrTrunc res2 Size8 (Loc res) Size1
        return (ABS.Bool, Loc res2)

transExpr (ABS.EString string) = do
    (StringDef addr ty _) <- (gets $ (Map.lookup string) . _stringsDefs) >>= \case
        Just strDef -> return strDef
        Nothing -> do
            addr <- gets $ GlobalIdent . makeStringIdent . _stringCounter
            strDef <- return $ StringDef addr (Arr ((+1) . toInteger . length $ string) Size8) string
            modify (\s -> s{_stringsDefs = Map.insert string strDef $ _stringsDefs s, _stringCounter = (_stringCounter s) + 1})
            return strDef
    res <- getNextLocalIdent
    emitInstr $ InstrGetElementPtr res ty (Ptr ty) (Glob addr) [ConstInt 0, ConstInt 0]
    return (ABS.Str, Loc res) 
  where
    makeStringIdent :: Integer -> String
    makeStringIdent int 
        | int == 0 = ".str"
        | otherwise = ".str." ++ (show int)

transExpr call@(ABS.ECall ident exprs) = asks (_currentClass) >>= getClassWithMethod ident >>= \case 
    Just _ -> transExpr (ABS.EMetCall (ABS.EVar ABS.LSelf) ident exprs)
    Nothing ->  (asks $ (Map.lookup ident) . _functions) >>= \case
        Nothing -> throwError $ GenMError "ERROR: Unknown function identifier"
        Just (Function _ type_ argTypes funAddr) -> do
            unless (length exprs == length argTypes) . throwError $ GenMError "ERROR: Wrong number of arguments"
            exprs <- zipWithM transExprCompatibleType argTypes exprs >>= mapM (uncurry zextBool)
            types <- mapM transType argTypes
            if type_ == ABS.Void then do
                emitInstr . InstrVoidCall funAddr $ zip types (map snd exprs)
                return (type_, Null)
            else do
                ty <- transType type_ 
                res <- getNextLocalIdent
                emitInstr . InstrCall res ty funAddr $ zip types (map snd exprs)
                if type_ /= ABS.Bool then return (type_, Loc res) else do
                    res2 <- getNextLocalIdent
                    emitInstr $ InstrTrunc res2 Size8 (Loc res) Size1
                    return (ABS.Bool, Loc res2)

transExpr (ABS.EMetCall expr ident exprs) = do
    (type_, operand) <- transExpr expr
    case type_ of
        ABS.Obj classIdent -> (asks $ (Map.lookup classIdent) . _classes) >>= getClassWithMethod ident >>= \case  
            Nothing -> throwError $ GenMError "ERROR: Unknown method name"
            Just class_ -> do
                (Function _ type_ argTypes funAddr) <- return . (Map.! ident) . _classMethods $ class_
                -- Copied code, move to function: transFunction
                unless (length exprs + 1 == length argTypes) . throwError $ GenMError "ERROR: Wrong number of arguments"
                exprs <- zipWithM transExprCompatibleType argTypes (expr:exprs) >>= mapM (uncurry zextBool)
                types <- mapM transType argTypes
                if type_ == ABS.Void then do
                    emitInstr . InstrVoidCall funAddr $ zip types (map snd exprs)
                    return (type_, Null)
                else do
                    ty <- transType type_ 
                    res <- getNextLocalIdent
                    emitInstr . InstrCall res ty funAddr $ zip types (map snd exprs)
                    if type_ /= ABS.Bool then return (type_, Loc res) else do
                        res2 <- getNextLocalIdent
                        emitInstr $ InstrTrunc res2 Size8 (Loc res) Size1
                        return (ABS.Bool, Loc res2)
        
        _ -> throwError $ GenMError "ERROR: Call on non class type"


transExpr (ABS.ENewObj ident) = (asks $ (Map.lookup ident) . _classes) >>= \case
    Nothing -> throwError $ GenMError "ERROR: Unknown class type"
    Just class_ -> do
        size <- computeClassSize class_
        ty <- return . TypeClass . _classAddr $ class_
        malloc <- asks $ _functionAddress . (Map.! (ABS.Ident "malloc")) . _functions
        memset <- asks $ _functionAddress . (Map.! (ABS.Ident "llvm.memset")) . _functions
        res1 <- getNextLocalIdent
        res2 <- getNextLocalIdent
        emitInstr $ InstrCall res1 (Ptr Size8) malloc [(Size32, ConstInt size)]
        emitInstr $ InstrVoidCall memset [(Ptr Size8, Loc res1), (Size8, ConstInt 0), (Size32, ConstInt size), (Size32, ConstInt 1), (Size1, ConstBool False)]
        emitInstr $ InstrBitcast res2 (Ptr Size8) (Loc res1) (Ptr ty)
        return (ABS.Obj ident, Loc res2)

  where
    computeClassSize :: GenM m => Class -> m Integer
    computeClassSize class_ = do
        superSize <- case _classParent class_ of
            Nothing -> return 0
            Just ident -> (asks $ (Map.! ident) . _classes) >>= computeClassSize
        return $ Map.foldr accumSize superSize (_classFields class_)
    
    accumSize :: (ABS.Type, Integer) -> Integer -> Integer
    accumSize field size = size + (getTypeSize . fst $ field)

transExpr (ABS.ENewArr type_ expr) = do
    operand <- transExprRequireType (ABS.Int) expr
    when (type_ == ABS.Void) . throwError $ GenMError "Error: void array type"
    malloc <- asks $ _functionAddress . (Map.! (ABS.Ident "malloc")) . _functions
    memset <- asks $ _functionAddress . (Map.! (ABS.Ident "llvm.memset")) . _functions

    res1 <- getNextLocalIdent
    res2 <- getNextLocalIdent
    res3 <- getNextLocalIdent
    res4 <- getNextLocalIdent
    
    emitInstr $ InstrBinOp res1 Times Size32 operand (ConstInt (getTypeSize type_))
    emitInstr $ InstrBinOp res2 Plus Size32 (Loc res1) (ConstInt (getTypeSize ABS.Int))
    emitInstr $ InstrCall res3 (Ptr Size8) malloc [(Size32, Loc res2)]
    emitInstr $ InstrVoidCall memset [(Ptr Size8, Loc res3), (Size8, ConstInt 0), (Size32, Loc res2), (Size32, ConstInt 1), (Size1, ConstBool False)]
    emitInstr $ InstrBitcast res4 (Ptr Size8) (Loc res3) (Ptr Size32)
    emitInstr $ InstrStore Size32 operand (Ptr Size32) res4
    return (ABS.Arr type_, Loc res3)

transExpr (ABS.Neg expr) = transExpr $ ABS.EMul expr ABS.Times (ABS.ELitInt (-1))
transExpr (ABS.Not expr) = transExpr $ ABS.ERel expr ABS.EQU (ABS.ELitFalse)

transExpr (ABS.EMul expr1 mulop expr2) = do
    operand1 <- transExprRequireType (ABS.Int) expr1
    operand2 <- transExprRequireType (ABS.Int) expr2
    case (operand1, mulop, operand2) of
        (ConstInt int1, ABS.Times, ConstInt int2) -> return (ABS.Int, ConstInt $ int1 * int2)
        (ConstInt int1, ABS.Div, ConstInt int2) -> return (ABS.Int, ConstInt $ int1 `div` int2)
        (ConstInt int1, ABS.Mod, ConstInt int2) -> return (ABS.Int, ConstInt $ int1 `mod` int2)
        _ -> do
            res <- getNextLocalIdent
            emitInstr $ InstrBinOp res (transMulOp mulop) Size32 operand1 operand2
            return (ABS.Int, Loc res)
  where
    transMulOp :: ABS.MulOp -> Op
    transMulOp ABS.Times = Times
    transMulOp ABS.Div = Div
    transMulOp ABS.Mod = Mod

transExpr (ABS.ECast ident expr) = do
    ty <- transType (ABS.Obj ident)
    (type_, op) <- transExprCompatibleType (ABS.Obj ident) expr
    return (type_, op)

transExpr (ABS.EAdd expr1 addop expr2) = do
    (type1, operand1) <- transExpr expr1
    (type2, operand2) <- transExpr expr2
    case (type1, addop, type2) of
        (ABS.Str, ABS.Plus, ABS.Str) -> do
            res <- getNextLocalIdent
            ty1 <- transType type1
            ty2 <- transType type2
            concat <- asks $ _functionAddress . (Map.! (ABS.Ident "concat")) . _functions
            emitInstr $ InstrCall res (Ptr Size8) concat [(ty1, operand1), (ty2, operand2)]
            return (ABS.Str, Loc res)
        (ABS.Int, _, ABS.Int) -> case (operand1, addop, operand2) of
            (ConstInt int1, ABS.Plus, ConstInt int2) -> return (ABS.Int, ConstInt $ int1 + int2)
            (ConstInt int1, ABS.Minus, ConstInt int2) -> return (ABS.Int, ConstInt $ int1 - int2)
            _ -> do
                res <- getNextLocalIdent
                emitInstr $ InstrBinOp res (transAddOp addop) Size32 operand1 operand2
                return (type1, Loc res)
        _ -> throwError $ GenMError "ERROR: Invalid types for add operation"
  where
    transAddOp :: ABS.AddOp -> Op
    transAddOp ABS.Plus = Plus
    transAddOp ABS.Minus = Minus

transExpr (ABS.ERel expr1 relop expr2) = do
    (type1, operand1) <- transExpr expr1
    operand2 <- transExprRequireType type1 expr2
    case (operand1, relop, operand2) of
        (ConstInt int1, relop, ConstInt int2) -> return (ABS.Bool, ConstBool $ computeRelop relop int1 int2)
        (ConstBool bool1, relop, ConstBool bool2) -> return (ABS.Bool, ConstBool $ computeRelop relop bool1 bool2)
        _ -> do 
            ty <- if (type1 == ABS.Bool) then return Size1 else transType type1
            res <- getNextLocalIdent
            emitInstr $ InstrCmp res (transRelOp relop) ty operand1 operand2
            return (ABS.Bool, Loc res)
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

transExpr (ABS.EAnd expr1 expr2) = do
    operand1 <- transExprRequireType ABS.Bool expr1
    case operand1 of
        ConstBool False -> return (ABS.Bool, operand1)
        ConstBool True -> do
            operand2 <- transExprRequireType ABS.Bool expr2
            return (ABS.Bool, operand2)
        _ -> do
            first <- gets _currentBlock
            mid <- newBlock
            modify $ \s -> s{_currentBlock = mid}
            operand2 <- transExprRequireType ABS.Bool expr2
            end <- newBlock
            emitEnd $ BlockEndBranch (LocalIdent . show $ end)
            (inBlock first) . emitEnd $ BlockEndBranchCond Size1 operand1 (LocalIdent . show $ mid) (LocalIdent . show $ end)

            modify $ \s -> s{_currentBlock = end}
            res <- getNextLocalIdent
            emitPhi $ Phi res Size1 [PhiBranch (LocalIdent . show $ first) (ConstBool False), PhiBranch (LocalIdent . show $ mid) operand2]
            return (ABS.Bool, Loc res)

transExpr (ABS.EOr expr1 expr2) = do
    operand1 <- transExprRequireType ABS.Bool expr1
    case operand1 of
        ConstBool True -> return (ABS.Bool, operand1)
        ConstBool False -> do
            operand2 <- transExprRequireType ABS.Bool expr2
            return (ABS.Bool, operand2)
        _ -> do
            first <- gets _currentBlock
            mid <- newBlock
            modify $ \s -> s{_currentBlock = mid}
            operand2 <- transExprRequireType ABS.Bool expr2
            end <- newBlock
            emitEnd $ BlockEndBranch (LocalIdent . show $ end)
            (inBlock first) . emitEnd $ BlockEndBranchCond Size1 operand1 (LocalIdent . show $ end) (LocalIdent . show $ mid)

            modify $ \s -> s{_currentBlock = end}
            res <- getNextLocalIdent
            emitPhi $ Phi res Size1 [PhiBranch (LocalIdent . show $ first) (ConstBool True), PhiBranch (LocalIdent . show $ mid) operand2]
            return (ABS.Bool, Loc res)


transLVal :: GenM m => ABS.LVal -> m (ABS.Type, LocalIdent)
transLVal (ABS.LVar ident) = asks (Map.lookup ident . _variables) >>= \case
    Nothing -> asks _currentClass >>= \case
        Nothing -> throwError $ GenMError "ERROR: Unknown variable identifier"
        Just _ -> transLVal $ (ABS.LAttr (ABS.EVar ABS.LSelf) ident)
    Just (Variable type_ addr) -> return (type_, addr)

transLVal (ABS.LArr expr1 expr2) = do
    operand2 <- transExprRequireType ABS.Int expr2
    (type_, operand1) <- transExpr expr1
    case type_ of
        ABS.Arr type_ -> do
            ty <- transType type_
            res1 <- getNextLocalIdent
            res2 <- getNextLocalIdent
            res3 <- getNextLocalIdent
            
            emitInstr $ InstrGetElementPtr res1 Size8 (Ptr Size8) operand1 [ConstInt 4]
            emitInstr $ InstrBitcast res2 (Ptr Size8) (Loc res1) (Ptr ty)
            emitInstr $ InstrGetElementPtr res3 ty (Ptr ty) (Loc res2) [operand2]
            return (type_ , res3)

        _ -> throwError $ GenMError "ERROR: Not an array type"

transLVal (ABS.LAttr expr ident) = do
    (type_, operand) <- transExpr expr
    case type_ of
        ABS.Arr _ -> do
            unless (ident == (ABS.Ident "length")) . throwError $ GenMError "ERROR: Access of different than 'length' array attribute"
            res <- getNextLocalIdent
            ty <- transType type_
            emitInstr $ InstrBitcast res ty operand (Ptr Size32)
            return (ABS.Int, res)

        ABS.Obj classIdent -> (asks $ (Map.lookup classIdent) . _classes) >>= getClassWithField ident >>= \case
            Nothing -> throwError $ GenMError "ERROR: Unknown identifier"
            Just class_ -> do    
                ty <- transType type_
                (Ptr ty, op) <- if _className class_ == classIdent then return (ty, operand)
                    else do
                        res <- getNextLocalIdent
                        ty2 <- return . TypeClass . _classAddr $ class_
                        emitInstr $ InstrBitcast res ty operand ty2
                        return $ (ty2, Loc res)
                (type_, nr) <- return . (Map.! ident) . _classFields $ class_
                res <- getNextLocalIdent
                emitInstr $ InstrGetElementPtr res ty (Ptr ty) op [ConstInt 0, ConstInt nr]
                return (type_, res)

        _ -> throwError $ GenMError "ERROR: Access attribute of non class or array type"

transLVal ABS.LSelf = (asks _currentClass) >>= \case
    Nothing -> throwError $ GenMError "Error: 'this' used outside class"
    Just class_ -> do
        ty <- return . TypeClass . _classAddr $ class_
        return (ABS.Obj . _className $ class_, LocalIdent "1") -- 'this' is alway first argument

transType :: GenM m => ABS.Type -> m Type
transType ABS.Bool  = return Size8
transType ABS.Int  = return Size32
transType ABS.Str  = return $ Ptr Size8
transType (ABS.Arr type_) = return $ Ptr Size8
transType ABS.Void = return Void
transType (ABS.Obj ident) = (asks $ (Map.lookup ident) . _classes) >>= \case
    Nothing -> throwError $ GenMError "ERROR: Unknown type"
    Just class_ -> return . Ptr . TypeClass . _classAddr $ class_

transExprCompatibleType :: GenM m => ABS.Type -> ABS.Expr -> m (ABS.Type, Operand)
transExprCompatibleType type_ expr = do
    (exprType, op) <- transExpr expr
    case (type_, exprType) of
        (ABS.Obj ident1, ABS.Obj ident2) -> do
            isAnscestor ident1 ident2 >>= flip unless (throwError $ GenMError "ERROR: Incompatible types classes")
            ty1 <- transType exprType
            ty2 <- transType type_
            res <- getNextLocalIdent
            emitInstr $ InstrBitcast res ty1 op ty2
            return (type_, Loc res)

        (ABS.Obj ident1, ABS.Null) -> return (type_, op)
        (ABS.Arr _, ABS.Null) -> return (type_, op)
        (ABS.Str, ABS.Null) -> return (type_, op)
        (type1, type2) -> if (type1 == type2) then return (exprType, op) else throwError $ GenMError "ERROR: Incompatible types"  
  where
    isAnscestor :: GenM m => ABS.Ident -> ABS.Ident -> m Bool
    isAnscestor ident1 ident2 = if (ident1 == ident2) then return True else
        (asks $ _classParent . (Map.! ident2) . _classes) >>= \case
            Nothing -> return False
            Just ident2 -> isAnscestor ident1 ident2


transExprRequireType :: GenM m => ABS.Type -> ABS.Expr -> m Operand
transExprRequireType type_ expr = do
    (exprType, operand) <- transExpr expr
    unless (type_ == exprType) . throwError $ GenMError "ERROR: Invalid type"
    return operand        

getTypeSize :: ABS.Type -> Integer
getTypeSize ABS.Int = 4
getTypeSize ABS.Bool = 1
getTypeSize ABS.Str = 1
getTypeSize (ABS.Arr _) = 1
getTypeSize (ABS.Obj _) = 1

zextBool :: GenM m => ABS.Type -> Operand -> m (ABS.Type, Operand)
zextBool ABS.Bool (ConstBool True) = return (ABS.Bool, ConstInt 1)
zextBool ABS.Bool (ConstBool False) = return (ABS.Bool, ConstInt 0)
zextBool ABS.Bool operand = do
    res <- getNextLocalIdent
    emitInstr $ InstrZext res Size1 operand Size8
    return (ABS.Bool, Loc res)
zextBool type_ op = return (type_, op)

getClassWithMethod :: GenM m => ABS.Ident -> Maybe Class -> m (Maybe Class)
getClassWithMethod _ Nothing = return Nothing
getClassWithMethod methodIdent (Just class_) = 
    if (Map.member methodIdent) . _classMethods $ class_ 
    then return . Just $ class_ 
    else case _classParent class_ of
        Nothing -> return Nothing
        Just ident -> (asks $ (Map.lookup ident) . _classes) >>= getClassWithMethod  methodIdent 

getClassWithField :: GenM m => ABS.Ident -> Maybe Class-> m (Maybe Class)
getClassWithField _ Nothing = return Nothing
getClassWithField fieldIdent (Just class_) =
    if (Map.member fieldIdent) . _classFields $ class_
    then return . Just $ class_
    else case _classParent class_ of
        Nothing -> return Nothing
        Just ident -> (asks $ (Map.lookup ident) . _classes) >>= getClassWithField  fieldIdent

newBlock :: GenM m => m Integer
newBlock = do
    uniqueId <- getNextId
    modify $ \s -> s{_blocks = Map.insert uniqueId (Block uniqueId Seq.empty Seq.empty BlockEndNone) (_blocks s)}
    return uniqueId

inBlock :: GenM m => Integer -> m a -> m a
inBlock nr act = do
    current <- gets _currentBlock
    modify $ \s -> s{_currentBlock = nr}
    ret <- act
    modify $ \s -> s{_currentBlock = current} 
    return ret

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

emitPhi :: GenM m => Phi -> m ()
emitPhi phi = do
    nr <- gets _currentBlock
    block <- gets $ (Map.! nr) . _blocks
    modify (\s -> s{_blocks = Map.insert nr (block{_blockPhi= phi Seq.<| _blockPhi block}) $ _blocks s})

emitEnd :: GenM m => BlockEnd -> m ()
emitEnd end = isReachable >>= flip when (do
    nr <- gets _currentBlock
    block <- gets $ (Map.! nr) . _blocks
    modify (\s -> s{_blocks = Map.insert nr (block{_blockEnd=end}) $ _blocks s}))

isReturn :: GenM m => m Bool
isReturn  = do
    nr <- gets _currentBlock
    (gets $ _blockEnd . (Map.! nr) . _blocks) >>= \case
        BlockEndReturn _ _ -> return True
        BlockEndReturnVoid -> return True
        _ -> return False

isReachable :: GenM m => m Bool
isReachable = do
    nr <- gets _currentBlock
    (gets $ _blockEnd . (Map.! nr) . _blocks) >>= \case
        BlockEndNone -> return True
        _ -> return False

transBlockLabel :: Integer -> LocalIdent
transBlockLabel = LocalIdent . show


-- voidRun :: GenM m => (() -> m ()) -> m ()
-- voidRun next = do
--     current <- get
--     next ()
--     modify (const current) 

                -- ContT $ \next -> voidRun $ \_ -> do
                --     end <- newBlock 
                --     modify $ \s -> s{_currentBlockLabel = end}
                --     next () 
