module Frontend.Frontend where
  
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Cont

import PrintLatte
import AbsLatte
import ErrM

data TType 
    = TVoid | TInt | TStr | TBool | TArr TType | TObj Ident
    deriving (Eq, Read)

instance Show TType where
    show TVoid = "void"
    show TInt = "int"
    show TStr = "string"
    show TBool = "boolean"
    show (TArr type_) = (show type_) ++ "[]"
    show (TObj (Ident str)) = str

getArrElementType :: TType -> Maybe TType
getArrElementType (TArr type_) = Just type_
getArrElementType _ = Nothing

data Variable = Var {
    varDepth :: Integer,
    varType :: TType
}

data Function = Fun {
    fname :: Ident,
    ret :: TType,
    args :: [TType]
}

data Class = Class {
    parent :: Maybe Ident,
    fields :: M.Map Ident TType,
    methods :: M.Map Ident Function
}

data Context = Ctxt {
    level :: Integer,
    retType :: TType,
    env :: Env
}

data Env = Env {
    variables :: M.Map Ident Variable,
    classes :: M.Map Ident Class,
    functions :: M.Map Ident Function
}

initEnv :: Env
initEnv = Env {
    variables = M.empty,
    classes = M.empty,
    functions = M.fromList [(ident, f) | f@(Fun ident _ _) <- builtins]
}

funMain :: Ident
funMain = Ident "main"

builtins :: [Function]
builtins = [
    (Fun (Ident "printInt") TVoid [TInt]),
    (Fun (Ident "printString") TVoid [TStr]),
    (Fun (Ident "error") TVoid []),
    (Fun (Ident "readInt") TInt []),
    (Fun (Ident "readString") TStr [])]

ttype :: Type a -> TType
ttype (Void _) = TVoid
ttype (Int _) = TInt
ttype (Str _) = TStr
ttype (Bool _) = TBool
ttype (Arr _ type_)  = TArr $ ttype type_

type Loc = Maybe (Int, Int)

data Error = 
    Error String
  | ErrorLn Loc String
  | InFunctionError String Error
  | InClassError String Error

instance Show Error where
    show (Error string) = "error: " ++ string ++ "\n"
    show (ErrorLn nr string) = (show $ fromJust nr) ++ ": error: " ++ string
    show (InFunctionError name error) = (show error) ++ "\tin function: " ++ name ++ "\n"
    show (InClassError name error) = (show error) ++ "\tin class: " ++ name ++ "\n"

noMainError :: Error
noMainError = Error $ "no main function found\n\tprogram requires main function with sygnature 'int main()'"

multipleClassDeclarationError :: Loc -> Ident -> Error
multipleClassDeclarationError nr (Ident name) = ErrorLn nr $ "multiple declarations of class '" ++ name ++ "'"

multipleFunctionDeclarationError :: Loc -> Ident -> Error
multipleFunctionDeclarationError nr (Ident name) = ErrorLn nr $ "multiple declarations of function '" ++ name ++ "'"

multipleFieldDeclarationError :: Loc -> Ident -> Error
multipleFieldDeclarationError nr (Ident name) = ErrorLn nr $ "multiple declarations of field '" ++ name ++ "'"

multipleMethodDeclarationError :: Loc -> Ident -> Error
multipleMethodDeclarationError nr (Ident name) = ErrorLn nr $ "multiple declarations of method '" ++ name ++ "'"

multipleArgumentDeclarationError :: Loc -> Ident -> Error
multipleArgumentDeclarationError nr (Ident name) = ErrorLn nr $ "multiple declarations of argument '" ++ name ++ "'"

voidFieldError :: Loc -> Error
voidFieldError nr = ErrorLn nr $ "class field may not have '" ++ (show TVoid) ++ "' type"

voidArgumentError :: Loc -> Error
voidArgumentError nr = ErrorLn nr $ "argument may not have '" ++ (show TVoid) ++ "' type"

noReturnStmtError :: Error
noReturnStmtError = Error $ "missing return statement"

inheritenceCycleError :: Error
inheritenceCycleError = Error $ "inheritence cycle detected"

noAncestorClassError :: Ident -> Error
noAncestorClassError (Ident name) = Error $ "no ancestor class named '" ++ name ++ "' declared"

overrideError :: Error
overrideError = Error $ "override error\n\toverriden methods must have the same signature" 

wrongReturnTypeError :: Loc -> TType -> TType -> Error
wrongReturnTypeError nr type1 type2 = ErrorLn nr $ "wrong return type\n\texpected type: '" ++ (show type1) ++ "', actual type: '" ++ (show type2) ++ "'"

voidRetInNonVoidError :: Error
voidRetInNonVoidError = Error $ "void return in non void function"

wrongTypesError :: Loc -> TType -> TType -> Error
wrongTypesError nr type1 type2 = ErrorLn nr $ "incompatible types\n\texpected type: '" ++ (show type1) ++ "', actual type: '" ++ (show type2) ++ "'"

wrongCondExprType :: Loc -> Error
wrongCondExprType nr = ErrorLn nr $ "condition expression must have type '" ++ (show TBool) ++ "'"

incrDecrError :: Loc -> Error
incrDecrError nr = ErrorLn nr $ "operand for unary operation ++ or -- must be type '" ++ (show TInt) ++ "'"

iterationError :: Loc -> TType -> Error
iterationError nr type_ = ErrorLn nr $ "unable to iterate over non array type '" ++ (show type_) ++ "'" 

variableRedefinitionError :: Loc -> Ident -> Error
variableRedefinitionError nr (Ident name) = ErrorLn nr $ "redefinition of variable '" ++ name ++ "'"

undeclaredIdentifierError :: Loc -> Ident -> Error
undeclaredIdentifierError nr (Ident name) = ErrorLn nr $ "use of undeclared identifier '" ++ name ++"'"

arrayIndexTypeError :: Loc -> Error
arrayIndexTypeError nr =  ErrorLn nr $ "array index must be type '" ++ (show TInt) ++ "'"

arrayTypeRequiredError :: Loc -> Error
arrayTypeRequiredError nr = ErrorLn nr $ "array type required"

arrayLengthTypeError :: Loc -> Error
arrayLengthTypeError nr = ErrorLn nr $ "array length must be type '" ++ (show TInt) ++ "'"

unknownTypeName :: Loc -> Ident -> Error
unknownTypeName nr (Ident name) = ErrorLn nr $ "unknown type name '" ++ name ++ "'" 

-- -----------------------------------------------------------------------------------------------

checkProgram :: Program Loc -> IO (Either Error ())
checkProgram program = runExceptT . void $ collectTopDefs program >>= typeCheckProgram program

----- Collect top level definitions --------------------------------------------------------------

collectTopDefs :: Program Loc -> ExceptT Error IO Env
collectTopDefs (Prog _ topdefs) = do 
    env <- execStateT (mapM_ collectTopDef topdefs) initEnv
    validateMain env
    return env

validateMain :: Env -> ExceptT Error IO ()
validateMain env = case M.lookup funMain $ functions env of
    Nothing -> throwError $ noMainError
    Just (Fun _ type_ args) -> unless (type_ == TInt && null args) $ throwError noMainError

collectTopDef :: TopDef Loc -> StateT Env (ExceptT Error IO) ()
collectTopDef (ClassDef nr ident items) = do
    classes <- gets classes
    when (M.member ident classes) . throwError $  multipleClassDeclarationError nr ident
    collectClass ident Nothing items

collectTopDef (ClassExtDef nr ident1 ident2 items) = do 
    classes <- gets classes
    when (M.member ident1 classes) . throwError $ multipleClassDeclarationError nr ident1
    collectClass ident1 (Just ident2) items

collectTopDef (TopFunDef nr fundef) = do
    functions <- gets functions
    fun@(Fun ident _ _) <- lift $ extractFunction fundef
    when (M.member ident functions) . throwError $ multipleFunctionDeclarationError nr ident
    modify $ \env -> env {functions = M.insert ident fun functions}
    
collectClass :: Ident -> Maybe Ident -> [ClassItemDef Loc] -> StateT Env (ExceptT Error IO) ()
collectClass ident@(Ident str) parent items = do
    class_ <- lift . execStateT (mapM_ collectClassItemDef items) $ Class parent M.empty M.empty
    modify $ \env -> env {classes = M.insert ident class_ $ classes env}
    `catchError` (\err -> throwError $ InClassError str err)

collectClassItemDef :: ClassItemDef Loc -> StateT Class (ExceptT Error IO) ()
collectClassItemDef (AttrDef nr type_ ident) = do
    fields <- gets fields
    when (M.member ident fields) . throwError $ multipleFieldDeclarationError nr ident
    modify $ \class_ -> class_ {fields = M.insert ident (ttype type_) fields}

collectClassItemDef (MethodDef nr fundef) = do
    methods <- gets methods
    fun@(Fun ident@(Ident str) _ _) <- lift $ extractFunction fundef
    when (M.member ident methods) . throwError $ multipleMethodDeclarationError nr ident
    modify $ \class_ -> class_ {methods = M.insert ident fun methods}

extractFunction :: FunDef Loc -> (ExceptT Error IO) Function
extractFunction fun@(FunDef _ type_ ident@(Ident str) args block) = do 
    types <- evalStateT (mapM extractArg args) S.empty
    return $ Fun ident (ttype type_) types
    `catchError` \err -> throwError $ InFunctionError str err 

extractArg :: Arg Loc -> StateT (S.Set Ident) (ExceptT Error IO) TType
extractArg (Ar nr type_ ident) = do
    args <- get
    when (S.member ident args) . throwError $ multipleArgumentDeclarationError nr ident
    modify $ S.insert ident
    return $ ttype type_

----- Type check top level difinitions -------------------------------------------------------------

typeCheckProgram :: Program Loc -> Env -> ExceptT Error IO ()
typeCheckProgram (Prog _ topdefs) = runReaderT (mapM_ typeCheckTopDef topdefs)

typeCheckTopDef :: TopDef Loc -> ReaderT Env (ExceptT Error IO) ()
typeCheckTopDef (TopFunDef _ fundef) = typeCheckFunDef fundef
typeCheckTopDef (ClassExtDef nr ident1 ident2 items) = typeCheckTopDef $ ClassDef nr ident1 items
typeCheckTopDef (ClassDef _ ident@(Ident str) items) = do
    env' <- evalStateT (getClassEnv $ Just ident) S.empty
    local (\env -> env' {
      functions = M.union (functions env) $ functions env'
    }) $ mapM_ typeCheckClassItemDef items
    `catchError` \err -> throwError $ InClassError str err

typeCheckClassItemDef :: ClassItemDef Loc -> ReaderT Env (ExceptT Error IO) ()
typeCheckClassItemDef (MethodDef _ fundef) = typeCheckFunDef fundef
typeCheckClassItemDef (AttrDef nr type_ ident) = 
    when ((ttype type_) == TVoid) . throwError $ voidFieldError nr

typeCheckFunDef :: FunDef Loc -> ReaderT Env (ExceptT Error IO) ()
typeCheckFunDef (FunDef nr type_ ident@(Ident str) args block) = do
    type_ <- typeCheckType type_
    vars <- M.fromList <$> mapM typeCheckArg args
    env <- asks $ \env ->  env {variables = M.union (variables env) vars} 
    retStm <- lift . runReaderT (execStateT (runContT (typeCheckBlock block) return) False) $ Ctxt 0 type_ env
    unless (type_ == TVoid || retStm) . throwError $ noReturnStmtError
    `catchError` \err -> throwError $ InFunctionError str err 
    
typeCheckArg :: Arg Loc -> ReaderT Env (ExceptT Error IO) (Ident, Variable)
typeCheckArg (Ar nr type_ ident) = do
    type_ <- typeCheckType type_
    when (type_ == TVoid) . throwError $ voidArgumentError nr
    return (ident, Var 0 type_)

getClassEnv :: Maybe Ident -> StateT (S.Set Ident) (ReaderT Env (ExceptT Error IO)) Env
getClassEnv Nothing = return $ Env M.empty M.empty M.empty
getClassEnv (Just ident) = do
    descendants <- get
    when (S.member ident descendants) . throwError $ inheritenceCycleError
    class_ <- asks $ M.lookup ident . classes 
    case class_ of
        Nothing -> throwError $ noAncestorClassError ident
        Just class_ -> do
            modify $ S.insert ident
            env <- getClassEnv $ parent class_
            (lift . local (const env)) . mapM_ validateOverride . M.elems $ methods class_
            return env {
                variables = M.union (variables env) $ M.map (Var 1) (fields class_),
                functions = M.union (functions env) (methods class_)
            }

validateOverride :: Function -> ReaderT Env (ExceptT Error IO) ()
validateOverride (Fun ident ttype ttypes) = do
    functions <- asks functions
    case M.lookup ident functions of
        Nothing -> return ()
        (Just (Fun ident2 ttype2 ttypes2)) -> 
            unless (ttype == ttype2 && ttypes == ttypes2) . throwError $ overrideError

-----Type check Statements -------------------------------------
  
typeCheckBlock :: Block Loc -> ContT () (StateT Bool (ReaderT Context (ExceptT Error IO))) ()
typeCheckBlock (Blk _ stmts) = do
    lift . local (\ctxt -> ctxt {level = level ctxt + 1}) $ runContT (mapM_ typeCheckStmt stmts) return
    ContT $ \next -> next ()

typeCheckStmt :: Stmt Loc -> ContT () (StateT Bool (ReaderT Context (ExceptT Error IO))) ()
typeCheckStmt (BStmt _ block) = typeCheckBlock block
typeCheckStmt (Empty _) = return ()
  
typeCheckStmt (Ret nr expr) = lift $ do
    env <- asks env
    retType <- asks retType
    exprType <- lift . lift $ runReaderT (typeCheckExpr expr) env
    unless (retType == exprType) . throwError $ wrongReturnTypeError nr retType exprType
    modify $ const True
  
typeCheckStmt (VRet nr) = lift $ do
    retType <- asks retType
    unless (retType == TVoid) . throwError $ voidRetInNonVoidError
    modify $ const True

typeCheckStmt (Ass nr lval expr) = lift $ do
    env <- asks env
    lvalType <- lift . lift $ runReaderT (typeCheckLVal lval) env
    exprType <- lift . lift $ runReaderT (typeCheckExpr expr) env
    unless (lvalType == exprType) . throwError $ wrongTypesError nr lvalType exprType

typeCheckStmt (Decl _ type_ items) = do
    lvl <- asks level
    env <- asks env
    type_ <- lift . lift . lift $ runReaderT (typeCheckType type_) env 
    env' <- lift . lift . lift $ runReaderT (execStateT (mapM_ typeCheckItem items) env) (type_, lvl)
    ContT $ \next -> local (\ctxt -> ctxt{env = env'}) $ next ()

typeCheckStmt (Cond nr expr stmt) = typeCheckStmt (CondElse nr expr stmt (Empty Nothing))      
typeCheckStmt (CondElse nr expr stmt1 stmt2) = lift $ do
    env <- asks env
    exprType <- lift . lift $ runReaderT (typeCheckExpr expr) env
    unless (exprType == TBool) . throwError $ wrongCondExprType nr
    retStm1 <- lift . local (\ctxt -> ctxt {level = level ctxt + 1}) $ execStateT (runContT (typeCheckStmt stmt1) return) False
    retStm2 <- lift . local (\ctxt -> ctxt {level = level ctxt + 1}) $ execStateT (runContT (typeCheckStmt stmt2) return) False
    modify $ \ret -> ret || retStm1 && retStm2

typeCheckStmt (While nr expr stmt) = lift $ do
    env <- asks env
    exprType <- lift . lift $ runReaderT (typeCheckExpr expr) env
    unless (exprType == TBool) . throwError $ wrongCondExprType nr
    local (\ctxt -> ctxt {level = level ctxt + 1}) $ runContT (typeCheckStmt stmt) return

typeCheckStmt (Decr nr lval) = typeCheckStmt (Incr nr lval)
typeCheckStmt (Incr nr lval) = lift $ do
    env <- asks env
    type_ <- lift . lift $ runReaderT (typeCheckLVal lval) env
    unless (type_ == TInt) . throwError $ incrDecrError nr

typeCheckStmt (For nr type_ ident expr stmt) = lift $ do
    lvl <- asks level
    env <- asks env
    type_ <- lift . lift $ runReaderT (typeCheckType type_) env
    expType <- lift . lift $ runReaderT (typeCheckExpr expr) env
    case getArrElementType expType of
        Nothing -> throwError $ iterationError nr expType
        Just arrElType -> do
            unless (arrElType == type_) . throwError $ wrongTypesError nr type_ arrElType
            ctxt' <- asks $ \ctxt -> ctxt{level = lvl + 2, env = env{variables = M.insert ident (Var (lvl + 1) type_) $ variables env}}
            retStm <- lift . local (const ctxt') $ execStateT (runContT (typeCheckStmt stmt) return) False
            modify $ \ret -> ret || retStm

typeCheckStmt (SExp _ expr) = lift $ do
    env <- asks env
    lift . lift . void $ runReaderT (typeCheckExpr expr) env

typeCheckItem :: Item Loc -> StateT Env (ReaderT (TType, Integer) (ExceptT Error IO)) ()
typeCheckItem (Init nr ident expr) = do
    env <- get
    type_ <- asks fst
    exprType <- lift . lift $ runReaderT (typeCheckExpr expr) env
    lift . unless (type_ == exprType) . throwError $ wrongTypesError nr type_ exprType
    typeCheckItem (NoInit nr ident)

typeCheckItem (NoInit nr ident) = do
    (type_, lvl) <- ask
    var <- gets $ M.lookup ident . variables
    case var of
        Just var -> when (varDepth var == lvl) . throwError $ variableRedefinitionError nr ident
        Nothing -> return ()
    modify $ \env -> env {variables = M.insert ident (Var lvl type_) $ variables env}
  

-- ------------------------------------ Exprs ---------------------------------------------

typeCheckLVal :: LVal Loc -> ReaderT Env (ExceptT Error IO) TType
typeCheckLVal (LVar nr ident) = do
    variables <- asks variables 
    unless (M.member ident variables) . throwError $ undeclaredIdentifierError nr ident
    return . varType $ (variables M.! ident)

typeCheckLVal (LArr nr expr1 expr2) = do
    exprType1 <- typeCheckExpr expr1
    exprType2 <- typeCheckExpr expr2
    unless (exprType2 == TInt) . throwError $ arrayIndexTypeError nr
    case getArrElementType exprType2 of
        Nothing -> throwError $ arrayTypeRequiredError nr
        Just type_ -> return type_

-- {- TODO:
--   - validate that type is obj
--   - validate if class exists
-- -}
-- -- Obj
-- typeCheckLVal (LAttr expr ident) = do
--     type_ <- typeCheckExpr expr
--     case type_ of
--         TObj ident -> do
--             variables <- variables <$> evalStateT (getClassEnv $ Just classIdent) S.empty
--             unless (M.member ident variables) $ throwError WrongTypes
--             return $ varType $ variables M.! classIdent
--         _ -> throwError 


-- {- TODO:
--   - add null type
-- -}
typeCheckExpr ::  Expr Loc -> ReaderT Env (ExceptT Error IO) TType
typeCheckExpr (ELitTrue _) = return TBool

typeCheckExpr (ELitFalse _) = return TBool

typeCheckExpr (EString _ string) = return TStr

typeCheckExpr (ELitInt _ integer) = return TInt

typeCheckExpr (EVar _ lval) = typeCheckLVal lval

-- check if not a basic type
typeCheckExpr (ENewObj nr ident) = do
    type_ <- typeCheckType $ Obj nr ident
    return $ type_

-- -- Check function arguments
-- typeCheckExpr (ECall ident exprs) = do
--     functions <- asks functions
--     unless (M.member ident functions) $ throwError WrongTypes
--     return . ret $ functions M.! ident

-- -- Check function arguments
-- -- Error if expr type is not Obj (class)
-- typeCheckExpr (EMetCall expr ident exprs) = do
--     (Obj classIdent) <- typeCheckExpr expr
--     functions <- functions <$> evalStateT (getClassEnv $ Just classIdent) S.empty
--     unless (M.member ident functions) $ throwError WrongTypes
--     return . ret $ functions M.! classIdent  

typeCheckExpr (ENewArr nr type_ expr) = do
    type_ <- typeCheckType type_
    expType <- typeCheckExpr expr
    unless (expType == TInt) . throwError $ arrayLengthTypeError nr
    return $ TArr type_

-- typeCheckExpr (Neg expr) = do
--     expType <- typeCheckExpr expr
--     unless (expType == TInt) $ throwError WrongTypes
--     return Int

-- typeCheckExpr (Not expr) = do
--     expType <- typeCheckExpr expr
--     unless (expType == Bool) $ throwError WrongTypes
--     return Bool

-- typeCheckExpr (EMul expr1 mulop expr2) = do
--     expType1 <- typeCheckExpr expr1
--     expType2 <- typeCheckExpr expr2
--     unless (expType1 == Int && expType2 == Int) $ throwError WrongTypes
--     return Int

-- typeCheckExpr (EAdd expr1 addop expr2) = do
--     expType1 <- typeCheckExpr expr1
--     expType2 <- typeCheckExpr expr2
--     unless (expType1 == Int && expType2 == Int) $ throwError WrongTypes
--     return Int
  
-- typeCheckExpr (ERel expr1 relop expr2) = do
--     expType1 <- typeCheckExpr expr1
--     expType2 <- typeCheckExpr expr2
--     unless (expType1 == expType2) $ throwError WrongTypes
--     return Bool
  
-- typeCheckExpr (EAnd expr1 expr2) = do
--     expType1 <- typeCheckExpr expr1
--     expType2 <- typeCheckExpr expr2
--     unless (expType1 == Bool && expType2 == Bool) $ throwError WrongTypes
--     return Bool

-- typeCheckExpr (EOr expr1 expr2) = do
--     expType1 <- typeCheckExpr expr1
--     expType2 <- typeCheckExpr expr2
--     unless (expType1 == Bool && expType2 == Bool) $ throwError WrongTypes
--     return Bool

-- {-
-- types TODO:
-- - Validate if declared
-- - Comparison of types for inheritence
-- -}

typeCheckType :: Type Loc -> ReaderT Env (ExceptT Error IO) TType
typeCheckType (Void _) = return TVoid
typeCheckType (Int _) = return TInt 
typeCheckType (Str _) = return TStr 
typeCheckType (Bool _) = return TBool 
typeCheckType (Arr _ type_) = TArr <$> typeCheckType type_
typeCheckType (Obj nr ident) = do
    classes <- asks classes
    unless (M.member ident classes) . throwError $ unknownTypeName nr ident
    return $ TObj ident