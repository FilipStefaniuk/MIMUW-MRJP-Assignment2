module Frontend where
    
    import qualified Data.Map as M
    import qualified Data.Set as S

    import Control.Monad.State
    import Control.Monad.Except
    import Control.Monad.Reader
    import Control.Monad.Cont
    
    import AbsLatte
    import ErrM
    type Result = Err String

    data Variable = Var {
      varDepth :: Integer,
      varType :: Type
    }

    data Function = Fun {
        ret :: Type,
        args :: [Type]
    }

    data Class = Class {
        parent :: Maybe Ident,
        fields :: M.Map Ident Type,
        methods :: M.Map Ident Function
    }

    data Context = Ctxt {
        level :: Integer,
        retType :: Type,
        environment :: Env
    }

    data Env = Env {
        variables :: M.Map Ident Variable,
        classes :: M.Map Ident Class,
        functions :: M.Map Ident Function
    }

    data CheckError = 
        InClassError Ident CheckError
      | InFunctionError Ident CheckError
      | MultipleFieldDeclarations Ident
      | MultipleMethodDeclarations Ident
      | MultipleFunctionDeclarations Ident
      | NoMainFunction
      | VoidArgument Ident 
      | MultipleArgumentDeclarations Ident
      | NoReturnStatement
      | VoidReturn
      | WrongTypes
      | ErrorTest String
      | VoidField Ident
      | WrongMainFunctionType
      | InheritenceError Ident
      | ConditionTypeError
      | IncDecTypeError
      | WrongReturnType Type Type
      | IncompatibleTypes Type Type
      | MultipleVariableDeclarations Ident
      | UndeclaredVariable Ident
      | InheritenceCycle

    instance Show CheckError where
      show (WrongTypes) = "wrongTypes"
      show (ErrorTest string) = "Testing: '" ++ string ++ "'"
      
      show InheritenceCycle = "inheritence cycle detected"
      show (UndeclaredVariable (Ident name)) = "variable '" ++ name ++ "' is undeclared"
      show (NoReturnStatement) = "missing return statement"
      show (InFunctionError (Ident name) err) = "in Function '" ++ name ++ "' " ++ (show err) 
      show (InClassError (Ident name) err) = "in Class '" ++ name ++ "' " ++ (show err)
      show (WrongReturnType ftype exprtype) = "return with type '" ++ (show exprtype) ++"' in function with return type '" ++ (show ftype) ++ "'"
      show (VoidReturn) = "void return in non void function"
      show (MultipleVariableDeclarations (Ident name)) = "multiple declarations of variable '" ++ name ++ "'"
      show (MultipleFieldDeclarations (Ident name)) = "multiple declarations of field '" ++ name ++ "'"
      show (MultipleMethodDeclarations (Ident name)) = "multiple declarations of method '" ++ name ++ "'"
      show (MultipleArgumentDeclarations (Ident name)) = "multiple declarations of argument '" ++ name ++ "'"
      show (MultipleFunctionDeclarations (Ident name)) = "multiple declarations of function '" ++ name ++ "'"
      show (VoidArgument (Ident name)) = "argument '" ++ name ++ "' declared as void"
      show (VoidField (Ident name)) = "field '" ++ name ++ "' declared as void"
      show NoMainFunction = "no main function declared"
      show WrongMainFunctionType = "main function should return int and have no arguments"
      show (InheritenceError (Ident name)) = "ancestor class '" ++ name ++ "' not declared"
      show ConditionTypeError = "condition expression must be type 'bool'"
      show IncDecTypeError = "operand for unary operation ++ or -- must be type 'int'"
      show (IncompatibleTypes type1 type2) = "incompatible types: '" ++ (show type1) ++ "' and '" ++ (show type2) ++ "'"

    predefinedFunctions :: M.Map Ident Function
    predefinedFunctions = M.fromList [
      (Ident "printInt", Fun Void [Int]),
      (Ident "printString", Fun Void [Str]),
      (Ident "error", Fun Void []),
      (Ident "readInt", Fun Int []),
      (Ident "readString", Fun Str [])]

    -----------------------------------------------------------------------------------------------
    checkProgram ::Program -> Either CheckError ()
    checkProgram program = runExcept $ collectTopDefs program >>= typeCheckProgram program

    -- Collect top level definitions --------------------------------------------------------------

    collectTopDefs :: Program -> Except CheckError Env
    collectTopDefs (Prog topdefs) = do 
      env <- execStateT (mapM_ collectTopDef topdefs) $ Env M.empty M.empty predefinedFunctions
      case M.lookup (Ident "main") $ functions env of
        Nothing -> throwError NoMainFunction
        Just (Fun type_ args) -> unless (type_ == Int && null args) $ throwError WrongMainFunctionType
      return env

    collectTopDef :: TopDef -> StateT Env (Except CheckError) ()
    collectTopDef (ClassDef ident items) = collectClass ident Nothing items
    collectTopDef (ClassExtDef ident1 ident2 items) = collectClass ident1 (Just ident2) items
    collectTopDef (TopFunDef fundef) = do
      functions <- gets functions
      (ident, fun) <- lift $ extractFunction fundef
      when (M.member ident functions) $ throwError $ MultipleFunctionDeclarations ident
      modify $ \env -> env {functions = M.insert ident fun functions}
      
    collectClass :: Ident -> Maybe Ident -> [ClassItemDef] -> StateT Env (Except CheckError) ()
    collectClass ident parent items = do
      class_ <- lift $ execStateT (mapM_ collectClassItemDef items) $ Class parent M.empty M.empty
      modify $ \env -> env {classes = M.insert ident class_ $ classes env}
      `catchError` \err -> throwError $ InClassError ident err

    collectClassItemDef :: ClassItemDef -> StateT Class (Except CheckError) ()
    collectClassItemDef (AttrDef type_ ident) = do
      fields <- gets fields
      when (M.member ident fields) $ throwError $ MultipleFieldDeclarations ident
      modify $ \class_ -> class_ {fields = M.insert ident type_ fields}

    collectClassItemDef (MethodDef fundef) = do
      methods <- gets methods
      (ident, fun) <- lift $ extractFunction fundef
      when (M.member ident methods) $ throwError $ MultipleMethodDeclarations ident
      modify $ \class_ -> class_ {methods = M.insert ident fun methods}

    extractFunction :: FunDef -> Except CheckError (Ident, Function)
    extractFunction (FunDef type_ ident args block) = do 
      types <- evalStateT (mapM extractArg args) S.empty
      return $ (ident, Fun type_ types)
      `catchError` \err -> throwError $ InFunctionError ident err 

    extractArg :: Arg -> StateT (S.Set Ident) (Except CheckError) Type
    extractArg (Ar type_ ident) = do
      args <- get
      when (S.member ident args) $ throwError $ MultipleArgumentDeclarations ident
      modify $ S.insert ident
      return type_

    -- Type check top level difinitions -------------------------------------------------------------

    typeCheckProgram :: Program -> Env -> Except CheckError ()
    typeCheckProgram (Prog topdefs) env = runReaderT (mapM_ typeCheckTopDef topdefs) env

    typeCheckTopDef :: TopDef -> ReaderT Env (Except CheckError) ()
    typeCheckTopDef (TopFunDef fundef) = typeCheckFunDef fundef
    typeCheckTopDef (ClassExtDef ident1 ident2 items) = typeCheckTopDef $ ClassDef ident1 items
    typeCheckTopDef (ClassDef ident items) = do
      env' <- evalStateT (getClassEnv $ Just ident) S.empty
      local (\env -> env' {
        functions = M.union (functions env) $ functions env'
      }) $ mapM_ typeCheckClassItemDef items
      `catchError` \err -> throwError $ InClassError ident err

    typeCheckClassItemDef :: ClassItemDef -> ReaderT Env (Except CheckError) ()
    typeCheckClassItemDef (MethodDef fundef) = typeCheckFunDef fundef
    typeCheckClassItemDef (AttrDef type_ ident) = when (type_ == Void) $ throwError $ VoidField ident    

    typeCheckFunDef :: FunDef -> ReaderT Env (Except CheckError) ()
    typeCheckFunDef (FunDef type_ ident args block) = do
      vars <- fmap M.fromList $ mapM typeCheckArg args
      env <- asks $ \env ->  env {variables = M.union (variables env) vars} 
      retStm <- lift $ runReaderT (execStateT (runContT (typeCheckBlock block) return) False) $ Ctxt 0 type_ env
      unless (type_ == Void || retStm) $ throwError NoReturnStatement
      `catchError` \err -> throwError $ InFunctionError ident err
      
    typeCheckArg :: Arg -> ReaderT Env (Except CheckError) (Ident, Variable)
    typeCheckArg (Ar type_ ident) = do
      when (type_ == Void) $ throwError $ VoidArgument ident
      return (ident, Var 0 type_)

    -- sprawdzanie sugnatur
    getClassEnv :: Maybe Ident -> StateT (S.Set Ident) (ReaderT Env (Except CheckError)) Env
    getClassEnv Nothing = return $ Env M.empty M.empty M.empty
    getClassEnv (Just ident) = do
      descendants <- get
      when (S.member ident descendants) $ throwError InheritenceCycle
      class_ <- asks $ (M.lookup ident) . classes 
      case class_ of
        Nothing -> throwError $ InheritenceError ident
        Just (class_) -> do
          modify $ S.insert ident
          env <- getClassEnv $ parent class_
          return env {
            variables = M.union (variables env) $ M.map (Var 1) (fields class_),
            functions = M.union (functions env) (methods class_)
          }

    --Type check Statements -------------------------------------
    
    typeCheckBlock :: Block -> ContT () (StateT Bool (ReaderT Context (Except CheckError))) ()
    typeCheckBlock (Blk stmts) = do
      lift $ local (\ctxt -> ctxt {level = level ctxt + 1}) $ runContT (mapM_ typeCheckStmt stmts) return
      ContT $ \next -> next ()

    typeCheckStmt :: Stmt -> ContT () (StateT Bool (ReaderT Context (Except CheckError))) ()

    -- block
    typeCheckStmt (BStmt block) = typeCheckBlock block
    
    -- empty
    typeCheckStmt (Empty) = ContT $ \next -> next ()
    
    -- ret
    typeCheckStmt (Ret expr) = do
      env <- asks environment
      retType <- asks retType
      exprType <- lift $ lift $ lift $ runReaderT (typeCheckExpr expr) env
      lift $ unless (retType == exprType) $ throwError $ WrongReturnType retType exprType
      modify $ const True
      ContT $ \next -> next () 
    
    -- ret void
    typeCheckStmt VRet = do
      retType <- asks retType
      lift $ unless (retType == Void) $ throwError $ VoidReturn
      modify $ const True
      ContT $ \next -> next ()
    
    -- assign
    typeCheckStmt (Ass lval expr) = do
      env <- asks environment
      lvalType <- lift $ lift $ lift $ runReaderT (typeCheckLVal lval) env
      exprType <- lift $ lift $ lift $ runReaderT (typeCheckExpr expr) env
      lift $ unless (lvalType == exprType) $ throwError $ IncompatibleTypes lvalType exprType
      ContT $ \next -> next ()

    -- decl
    typeCheckStmt (Decl type_ items) = do
      lvl <- asks level
      env <- asks environment
      env' <- lift $ lift $ lift $ runReaderT (execStateT (mapM_ typeCheckItem items) env) (type_, lvl)
      ContT $ \next -> local (\ctxt -> ctxt{environment = env'}) $ next ()
    
    -- if
    typeCheckStmt (Cond expr stmt) = typeCheckStmt (CondElse expr stmt Empty)      
    typeCheckStmt (CondElse expr stmt1 stmt2) = do
      env <- asks environment
      exprType <- lift $ lift $ lift $ runReaderT (typeCheckExpr expr) env
      lift $ unless (exprType == Bool) $ throwError $ ConditionTypeError
      retStm1 <- lift $ lift $ local (\ctxt -> ctxt {level = level ctxt + 1}) $ execStateT (runContT (typeCheckStmt stmt1) return) False
      retStm2 <- lift $ lift $ local (\ctxt -> ctxt {level = level ctxt + 1}) $ execStateT (runContT (typeCheckStmt stmt2) return) False
      modify $ \ret -> ret || retStm1 && retStm2
      ContT $ \next -> next ()

    -- while
    typeCheckStmt (While expr stmt) = do
      env <- asks environment
      exprType <- lift $ lift $ lift $ runReaderT (typeCheckExpr expr) env
      lift $ unless (exprType == Bool) $ throwError $ ConditionTypeError
      lift $ local (\ctxt -> ctxt {level = level ctxt + 1}) $ runContT (typeCheckStmt stmt) return
      ContT $ \next -> next ()

    -- inc/dec
    typeCheckStmt (Decr lval) = typeCheckStmt (Incr lval)
    typeCheckStmt (Incr lval) = do
      env <- asks environment
      type_ <- lift $ lift $ lift $ runReaderT (typeCheckLVal lval) env
      lift $ unless (type_ == Int) $ throwError $ IncDecTypeError
      ContT $ \next -> next ()

    {- TODO:
      - check types, not equal but element of array
    -}
    -- for
    typeCheckStmt (For type_ ident expr stmt) = do
      lvl <- asks level
      env <- asks environment
      expType <- lift $ lift $ lift $ runReaderT (typeCheckExpr expr) env
      lift $ unless (expType == type_) $ throwError $ WrongTypes
      ctxt' <- asks $ \ctxt -> ctxt{level = lvl + 2, environment = env{variables = M.insert ident (Var (lvl + 1) type_) $ variables env}}
      retStm <- lift $ lift $ local (const ctxt') $ execStateT (runContT (typeCheckStmt stmt) return) False
      modify $ \ret -> ret || retStm
      ContT $ \next -> next () 
    
    -- expr
    typeCheckStmt (SExp expr) = do
      env <- asks environment
      lift $ lift $ lift $ runReaderT (typeCheckExpr expr) env
      ContT $ \next -> next ()

    typeCheckItem :: Item -> StateT Env (ReaderT (Type, Integer) (Except CheckError)) ()
    typeCheckItem (Init ident expr) = do
      env <- get
      type_ <- asks fst
      exprType <- lift $ lift $ runReaderT (typeCheckExpr expr) env
      unless (type_ == exprType) $ throwError $ IncompatibleTypes type_ exprType
      typeCheckItem (NoInit ident)

    typeCheckItem (NoInit ident) = do
      (type_, lvl) <- ask
      var <- gets $ (M.lookup ident) . variables
      case var of
        Just var -> when (varDepth var == lvl) $ throwError $ MultipleVariableDeclarations ident
        Nothing -> return ()
      modify $ \env -> env {variables = M.insert ident (Var lvl type_) $ variables env}
      

------------------------------------ Exprs ---------------------------------------------
    
    typeCheckLVal :: LVal -> ReaderT Env (Except CheckError) Type
    typeCheckLVal (LVar ident) = do
      variables <- asks variables 
      unless (M.member ident variables) $ throwError $ UndeclaredVariable ident
      return $ varType $ variables M.! ident

    {- TODO:
      - type must be array
      - return type of array element
    -}
    -- arrEl
    typeCheckLVal (LArr expr1 expr2) = do
      exprType1 <- typeCheckExpr expr1
      exprType2 <- typeCheckExpr expr2
      unless (exprType2 == Int) $ throwError WrongTypes
      return exprType1

    {- TODO:
      - validate that type is obj
      - validate if class exists
    -}
    -- Obj
    typeCheckLVal (LAttr expr ident) = do
      (Obj classIdent) <- typeCheckExpr expr
      variables <- fmap variables $ evalStateT (getClassEnv $ Just classIdent) S.empty
      unless (M.member ident variables) $ throwError WrongTypes
      return $ varType $ variables M.! classIdent

    
    {- TODO:
      - add null type
    -}
    typeCheckExpr :: Expr -> ReaderT Env (Except CheckError) Type
    -- true
    typeCheckExpr (ELitTrue) = return Bool
    
    -- false
    typeCheckExpr (ELitFalse) = return Bool
    
    -- string
    typeCheckExpr (EString string) = return Str
    
    -- int
    typeCheckExpr (ELitInt integer) = return Int
    
    -- lval
    typeCheckExpr (EVar lval) = typeCheckLVal lval
    
    typeCheckExpr (ENewObj ident) = do
      classes <- asks classes
      unless (M.member ident classes) $ throwError WrongTypes
      return $ Obj ident

    -- Check function arguments
    typeCheckExpr (ECall ident exprs) = do
      functions <- asks functions
      unless (M.member ident functions) $ throwError WrongTypes
      return $ ret $ functions M.! ident

    -- Check function arguments
    -- Error if expr type is not Obj (class)
    typeCheckExpr (EMetCall expr ident exprs) = do
      (Obj classIdent) <- typeCheckExpr expr
      functions <- fmap functions $ evalStateT (getClassEnv $ Just classIdent) S.empty
      unless (M.member ident functions) $ throwError WrongTypes
      return $ ret $ functions M.! classIdent  

    typeCheckExpr (ENewArr type_ expr) = do
      expType <- typeCheckExpr expr
      unless (expType == Int) $ throwError WrongTypes
      return type_

    ---------------------

    typeCheckExpr (Neg expr) = do
      expType <- typeCheckExpr expr
      unless (expType == Int) $ throwError WrongTypes
      return Int
    
    typeCheckExpr (Not expr) = do
      expType <- typeCheckExpr expr
      unless (expType == Bool) $ throwError WrongTypes
      return Bool
    
    typeCheckExpr (EMul expr1 mulop expr2) = do
      expType1 <- typeCheckExpr expr1
      expType2 <- typeCheckExpr expr2
      unless (expType1 == Int && expType2 == Int) $ throwError WrongTypes
      return Int

    typeCheckExpr (EAdd expr1 addop expr2) = do
      expType1 <- typeCheckExpr expr1
      expType2 <- typeCheckExpr expr2
      unless (expType1 == Int && expType2 == Int) $ throwError WrongTypes
      return Int
      
    typeCheckExpr (ERel expr1 relop expr2) = do
      expType1 <- typeCheckExpr expr1
      expType2 <- typeCheckExpr expr2
      unless (expType1 == expType2) $ throwError WrongTypes
      return Bool
      
    typeCheckExpr (EAnd expr1 expr2) = do
      expType1 <- typeCheckExpr expr1
      expType2 <- typeCheckExpr expr2
      unless (expType1 == Bool && expType2 == Bool) $ throwError WrongTypes
      return Bool

    typeCheckExpr (EOr expr1 expr2) = do
      expType1 <- typeCheckExpr expr1
      expType2 <- typeCheckExpr expr2
      unless (expType1 == Bool && expType2 == Bool) $ throwError WrongTypes
      return Bool


{-
types TODO:
  - Validate if declared
  - Comparison of types for inheritence
-}