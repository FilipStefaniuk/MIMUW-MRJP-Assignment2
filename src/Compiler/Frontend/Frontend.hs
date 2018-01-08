module Frontend.Frontend where
  
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Cont

import AbsLatte
import LexLatte
import ParLatte
import ErrM

import Frontend.Types
import Frontend.Error

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

data Env = Env {
    variables :: M.Map Ident Variable,
    classes :: M.Map Ident Class,
    functions :: M.Map Ident Function
}

data Context = Ctxt {
    level :: Integer,
    self :: Maybe Ident,
    retType :: TType,
    env :: Env
}

---- Constants ------------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------------------------

checkProgram :: String -> IO (Either Error (Program Loc))
checkProgram input = runExceptT $ do
    program <- parseProgram input
    env <- collectTopDefs program
    typeCheckProgram program env
    return program

----- Parse program -------------------------------------------------------------------------------

parseProgram :: String -> ExceptT Error IO (Program Loc)
parseProgram input = case pProgram $ myLexer input of
    Bad e -> throwError . Error $ e
    Ok a -> return a

----- Collect top level definitions ---------------------------------------------------------------

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
typeCheckProgram (Prog _ topdefs) env = runReaderT (mapM_ typeCheckTopDef topdefs) $ Ctxt 0 Nothing TVoid env

typeCheckTopDef :: TopDef Loc -> ReaderT Context (ExceptT Error IO) ()
typeCheckTopDef (TopFunDef _ fundef) = typeCheckFunDef fundef
typeCheckTopDef (ClassExtDef nr ident1 ident2 items) = typeCheckTopDef $ ClassDef nr ident1 items
typeCheckTopDef (ClassDef _ ident@(Ident str) items) = do
    env' <- evalStateT (getClassEnv $ Just ident) S.empty
    local (\ctxt -> ctxt { level = level ctxt + 1, env = env' {
      functions = M.union (functions . env $ ctxt) $ functions env'
    }}) $ mapM_ typeCheckClassItemDef items
    `catchError` \err -> throwError $ InClassError str err

typeCheckClassItemDef :: ClassItemDef Loc -> ReaderT Context (ExceptT Error IO) ()
typeCheckClassItemDef (MethodDef _ fundef) = typeCheckFunDef fundef
typeCheckClassItemDef (AttrDef nr type_ ident) = 
    when ((ttype type_) == TVoid) . throwError $ voidFieldError nr

typeCheckFunDef :: FunDef Loc -> ReaderT Context (ExceptT Error IO) ()
typeCheckFunDef (FunDef nr type_ ident@(Ident str) args block) = do
    type_ <- typeCheckType type_
    args <- M.fromList <$> mapM typeCheckArg args
    vars <- asks $ variables . env
    retStm <- local (\ctxt -> ctxt { 
        retType = type_, 
        env = (env ctxt) {variables = M.union vars args }}) 
        $ execStateT (runContT (typeCheckBlock block) return) False
    unless (type_ == TVoid || retStm) . throwError $ noReturnStmtError
    `catchError` \err -> throwError $ InFunctionError str err 
    
typeCheckArg :: Arg Loc -> ReaderT Context (ExceptT Error IO) (Ident, Variable)
typeCheckArg (Ar nr type_ ident) = do
    type_ <- typeCheckType type_
    when (type_ == TVoid) . throwError $ voidArgumentError nr
    lvl <- asks level
    return (ident, Var lvl type_)

getClassEnv :: Maybe Ident -> StateT (S.Set Ident) (ReaderT Context (ExceptT Error IO)) Env
getClassEnv Nothing = return $ Env M.empty M.empty M.empty
getClassEnv (Just ident) = do
    descendants <- get
    when (S.member ident descendants) . throwError $ inheritenceCycleError
    class_ <- asks $ (M.lookup ident . classes) . env 
    case class_ of
        Nothing -> throwError $ noAncestorClassError ident
        Just class_ -> do
            modify $ S.insert ident
            env' <- getClassEnv . parent $ class_
            lift . local (\ctxt -> ctxt{env = env'}) . mapM_ validateOverride . M.elems $ methods class_
            lvl <- asks $ level
            return env' {
                variables = M.union (variables env') $ M.map (Var lvl) (fields class_),
                functions = M.union (functions env') (methods class_)
            }

validateOverride :: Function -> ReaderT Context (ExceptT Error IO) ()
validateOverride (Fun ident ttype ttypes) = do
    functions <- asks $ functions . env
    case M.lookup ident functions of
        Nothing -> return ()
        (Just (Fun ident2 ttype2 ttypes2)) -> 
            unless (ttype == ttype2 && ttypes == ttypes2) . throwError $ overrideError

-----Type check Statements ------------------------------------------------------------------------
  
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
    exprType <- lift $ typeCheckExpr expr
    compatibleTypes <- lift $ isCompatible retType exprType
    unless (compatibleTypes) . throwError $ wrongReturnTypeError nr retType exprType
    modify $ const True
  
typeCheckStmt (VRet nr) = lift $ do
    retType <- asks retType
    unless (retType == TVoid) . throwError $ voidRetInNonVoidError
    modify $ const True

typeCheckStmt (Ass nr lval expr) = lift $ do
    env <- asks env
    lvalType <- lift $ typeCheckLVal lval
    exprType <- lift $ typeCheckExpr expr
    compatibleTypes <- lift $ isCompatible lvalType exprType
    unless (compatibleTypes) . throwError $ wrongTypesError nr lvalType exprType

typeCheckStmt (Decl _ type_ items) = do
    lvl <- asks level
    env <- asks env
    type_ <- lift . lift $ typeCheckType type_ 
    env' <- lift . lift $ runReaderT (execStateT (mapM_ typeCheckItem items) env) (type_, lvl)
    ContT $ \next -> local (\ctxt -> ctxt{env = env'}) $ next ()

typeCheckStmt (Cond nr expr stmt) = typeCheckStmt (CondElse nr expr stmt (Empty Nothing))      
typeCheckStmt (CondElse nr expr stmt1 stmt2) = lift $ do
    env <- asks env
    exprType <- lift $ typeCheckExpr expr
    unless (exprType == TBool) . throwError $ wrongCondExprType nr
    retStm1 <- lift . local (\ctxt -> ctxt {level = level ctxt + 1}) $ execStateT (runContT (typeCheckStmt stmt1) return) False
    retStm2 <- lift . local (\ctxt -> ctxt {level = level ctxt + 1}) $ execStateT (runContT (typeCheckStmt stmt2) return) False
    modify $ \ret -> ret || retStm1 && retStm2

typeCheckStmt (While nr expr stmt) = lift $ do
    env <- asks env
    exprType <- lift $ typeCheckExpr expr
    unless (exprType == TBool) . throwError $ wrongCondExprType nr
    local (\ctxt -> ctxt {level = level ctxt + 1}) $ runContT (typeCheckStmt stmt) return

typeCheckStmt (Decr nr lval) = typeCheckStmt (Incr nr lval)
typeCheckStmt (Incr nr lval) = lift $ do
    env <- asks env
    type_ <- lift $ typeCheckLVal lval
    unless (type_ == TInt) . throwError $ incrDecrError nr

typeCheckStmt (For nr type_ ident expr stmt) = lift $ do
    lvl <- asks level
    env <- asks env
    type_ <- lift $ typeCheckType type_
    expType <- lift $ typeCheckExpr expr
    case getArrElementType expType of
        Nothing -> throwError $ iterationError nr expType
        Just arrElType -> do
            compatibleTypes <- lift $ isCompatible type_ arrElType
            unless (compatibleTypes) . throwError $ wrongTypesError nr type_ arrElType
            ctxt' <- asks $ \ctxt -> ctxt{level = lvl + 2, env = env{variables = M.insert ident (Var (lvl + 1) type_) $ variables env}}
            retStm <- lift . local (const ctxt') $ execStateT (runContT (typeCheckStmt stmt) return) False
            modify $ \ret -> ret || retStm

typeCheckStmt (SExp _ expr) = lift $ do
    env <- asks env
    lift . void $ typeCheckExpr expr

typeCheckItem :: Item Loc -> StateT Env (ReaderT (TType, Integer) (ReaderT Context (ExceptT Error IO))) ()
typeCheckItem (Init nr ident expr) = do
    env <- get
    type_ <- asks fst
    exprType <- lift . lift $ typeCheckExpr expr
    compatibleTypes <- lift . lift $ isCompatible type_ exprType
    lift . unless (compatibleTypes) . throwError $ wrongTypesError nr type_ exprType
    typeCheckItem (NoInit nr ident)

typeCheckItem (NoInit nr ident) = do
    (type_, lvl) <- ask
    var <- gets $ M.lookup ident . variables
    case var of
        Just var -> when (varDepth var == lvl) . throwError $ variableRedefinitionError nr ident
        Nothing -> return ()
    modify $ \env -> env {variables = M.insert ident (Var lvl type_) $ variables env}
  

-------------------------------------- Exprs ------------------------------------------------------

typeCheckLVal :: LVal Loc -> ReaderT Context (ExceptT Error IO) TType
typeCheckLVal (LVar nr ident) = do
    variables <- asks $ variables . env 
    unless (M.member ident variables) . throwError $ undeclaredIdentifierError nr ident
    return . varType $ (variables M.! ident)

typeCheckLVal (LArr nr expr1 expr2) = do
    exprType1 <- typeCheckExpr expr1
    exprType2 <- typeCheckExpr expr2
    unless (exprType2 == TInt) . throwError $ arrayIndexTypeError nr
    case getArrElementType exprType2 of
        Nothing -> throwError $ arrayTypeRequiredError nr
        Just type_ -> return type_

typeCheckLVal (LAttr nr expr ident) = do
    type_ <- typeCheckExpr expr
    case type_ of
        TObj classIdent -> do
            variables <- variables <$> evalStateT (getClassEnv $ Just classIdent) S.empty
            case M.lookup ident variables of
                Nothing -> throwError $ noFieldError nr classIdent ident
                Just var -> return $ varType var
        _ -> throwError $ classTypeError nr

typeCheckLVal (LSelf nr) = do
    self <- asks self
    case self of
        Nothing -> throwError $ thisOutsideClassError nr
        Just ident -> return $ TObj ident

typeCheckExpr ::  Expr Loc -> ReaderT Context (ExceptT Error IO) TType
typeCheckExpr (ELitTrue _) = return TBool

typeCheckExpr (ELitFalse _) = return TBool

typeCheckExpr (EString _ string) = return TStr

typeCheckExpr (ELitInt _ integer) = return TInt

typeCheckExpr (Null _) = return TNull

typeCheckExpr (EVar _ lval) = typeCheckLVal lval

typeCheckExpr (ENewObj nr ident) = do
    type_ <- typeCheckType $ Obj nr ident
    return $ type_

typeCheckExpr (ECall nr ident exprs) = do
    functions <- asks $ functions . env
    case M.lookup ident functions of
        Nothing -> throwError $ undeclaredIdentifierError nr ident 
        Just fun -> do
            types <- mapM typeCheckExpr exprs
            compatibleTypes <- and <$> zipWithM isCompatible (args fun) types
            unless (compatibleTypes && (length (args fun) == length types)) . throwError $ functionCallError nr ident
            return . ret $ fun

typeCheckExpr (EMetCall nr expr ident exprs) = do
    type_ <- typeCheckExpr expr
    case type_ of
        TObj classIdent -> do
            functions <- functions <$> evalStateT (getClassEnv $ Just classIdent) S.empty
            case M.lookup ident functions of
                Nothing -> throwError $ noMethodError nr classIdent ident
                Just fun -> do
                    types <- mapM typeCheckExpr exprs
                    compatibleTypes <- and <$> zipWithM isCompatible (args fun) types
                    unless (compatibleTypes && (length (args fun) == length types)) . throwError $ methodCallError nr ident
                    return . ret $ fun  
        _ -> throwError $ classTypeError nr

typeCheckExpr (ENewArr nr type_ expr) = do
    type_ <- typeCheckType type_
    expType <- typeCheckExpr expr
    unless (expType == TInt) . throwError $ arrayLengthTypeError nr
    return $ TArr type_

typeCheckExpr (Neg nr expr) = do
    expType <- typeCheckExpr expr
    unless (expType == TInt) . throwError $ unaryOperatorTypeError nr expType "-"
    return TInt

typeCheckExpr (Not nr expr) = do
    expType <- typeCheckExpr expr
    unless (expType == TBool) . throwError $ unaryOperatorTypeError nr expType "!"
    return TBool

typeCheckExpr (EMul nr expr1 mulop expr2) = do
    expType1 <- typeCheckExpr expr1
    expType2 <- typeCheckExpr expr2
    unless (expType1 == TInt && expType2 == TInt) . throwError $ binaryOperatorTypesError nr expType1 expType2 (showMulOp mulop)
    return TInt

typeCheckExpr (EAdd nr expr1 addop expr2) = do
    expType1 <- typeCheckExpr expr1
    expType2 <- typeCheckExpr expr2
    unless (expType1 == TInt && expType2 == TInt) . throwError $ binaryOperatorTypesError nr expType1 expType2 (showAddOp addop)
    return TInt
  
typeCheckExpr (ERel nr expr1 relop expr2) = do
    expType1 <- typeCheckExpr expr1
    expType2 <- typeCheckExpr expr2
    unless (compare relop expType1 expType2) . throwError $ binaryOperatorTypesError nr expType1 expType2 (showRelOp relop)
    return TBool
    where
        compare _ TVoid _ = False
        compare _ _ TVoid = False
        compare _ TInt TInt = True
        compare _ TBool TBool = True
        compare (EQU _) a b = a == b
        compare (NE _) a b = a == b
        compare _ _ _ = False 

typeCheckExpr (EAnd nr expr1 expr2) = do
    expType1 <- typeCheckExpr expr1
    expType2 <- typeCheckExpr expr2
    unless (expType1 == TBool && expType2 == TBool) . throwError $ binaryOperatorTypesError nr expType1 expType2 "&&"
    return TBool

typeCheckExpr (EOr nr expr1 expr2) = do
    expType1 <- typeCheckExpr expr1
    expType2 <- typeCheckExpr expr2
    unless (expType1 == TBool && expType2 == TBool) . throwError $ binaryOperatorTypesError nr expType1 expType2 "||"
    return TBool

typeCheckType :: Type Loc -> ReaderT Context (ExceptT Error IO) TType
typeCheckType (Void _) = return TVoid
typeCheckType (Int _) = return TInt 
typeCheckType (Str _) = return TStr 
typeCheckType (Bool _) = return TBool 
typeCheckType (Arr _ type_) = TArr <$> typeCheckType type_
typeCheckType (Obj nr ident) = do
    classes <- asks $ classes . env
    unless (M.member ident classes) . throwError $ unknownTypeName nr ident
    return $ TObj ident

showAddOp :: AddOp a -> String
showAddOp (Plus _) = "+"
showAddOp (Minus _) = "-"

showMulOp :: MulOp a -> String
showMulOp (Times _) = "*"
showMulOp (Div _) = "/"
showMulOp (Mod _) = "%"

showRelOp :: RelOp a -> String
showRelOp (LTH _) = "<"
showRelOp (LE _) =  "<="
showRelOp (GTH _) = ">"
showRelOp (GE _) = ">="
showRelOp (EQU _) = "=="
showRelOp (NE _) = "!="

isCompatible :: TType -> TType -> ReaderT Context (ExceptT Error IO) Bool
isCompatible (TObj _) TNull = return True
isCompatible (TObj ident1) (TObj ident2) = do
    ancestors <- execStateT (getAncestors (Just ident2)) S.empty
    return $ S.member ident1 ancestors
isCompatible type1 type2 = return $ type1 == type2


getAncestors :: (Maybe Ident) -> StateT (S.Set Ident) (ReaderT Context (ExceptT Error IO)) ()
getAncestors Nothing = return ()
getAncestors (Just ident) = do
    classes <- asks $ classes . env
    case M.lookup ident classes of
        Nothing -> throwError $ unknownClassName ident
        (Just class_) -> do
            idents <- get
            when (S.member ident idents) . throwError $ inheritenceCycleError
            modify $ S.insert ident
            getAncestors $ parent class_
