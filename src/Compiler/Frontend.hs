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
      | VoidVariable Ident 
      | DuplicateArgumentName Ident
      | NoReturnStatement
      | VoidReturn
      | WrongTypes
      | DoubleDeclaration
      | ErrorTest String
    
    instance Show CheckError where
      show (InClassError (Ident name) err) = "inClass '" ++ name ++ "' " ++ (show err)
      show (InFunctionError (Ident name) err) = "in Function '" ++ name ++ "' " ++ (show err) 
      show (VoidVariable (Ident name)) = "variable '" ++ name ++ "' declared as void"
      show (DuplicateArgumentName (Ident name)) = "duplicate argument name '" ++ name ++ "'"
      show (NoReturnStatement) = "missing return statement"
      show (VoidReturn) = "void return in non void function"
      show (WrongTypes) = "wrongTypes"
      show (DoubleDeclaration) = "multiple declaration of same variable in block"
      show (ErrorTest string) = "Testing: '" ++ string ++ "'" 

    -----------------------------------------------------------------------------------------------
    checkProgram ::Program -> Either CheckError ()
    checkProgram program = runExcept $ collectTopDefs program >>= typeCheckProgram program

    -----------------------------------------------------------------------------------------------

    collectTopDefs :: Program -> Except CheckError Env
    collectTopDefs (Prog topdefs) = execStateT (mapM_ collectTopDef topdefs) $ Env M.empty M.empty M.empty

    collectTopDef :: TopDef -> StateT Env (Except CheckError) ()
    collectTopDef (ClassDef ident items) = collectClass ident items $ Class Nothing M.empty M.empty
    collectTopDef (ClassExtDef ident1 ident2 items) = collectClass ident1 items $ Class (Just ident2) M.empty M.empty
    collectTopDef (TopFunDef fundef) = do
      (ident, fun) <- lift $ extractFunction fundef 
      modify $ \env -> env {functions = M.insert ident fun $ functions env}
      
    collectClass :: Ident -> [ClassItemDef] -> Class -> StateT Env (Except CheckError) ()
    collectClass ident items emptyclass = do
      class_ <- lift $ execStateT (mapM_ collectClassItemDef items) emptyclass 
      modify $ \env -> env {classes = M.insert ident class_ $ classes env}
      `catchError` \err -> throwError $ InClassError ident err

    collectClassItemDef :: ClassItemDef -> StateT Class (Except CheckError) ()
    collectClassItemDef (AttrDef type_ ident) = modify $ \class_ -> class_ {fields = M.insert ident type_ $ fields class_}
    collectClassItemDef (MethodDef fundef) = do
      (ident, fun) <- lift $ extractFunction fundef
      modify $ \class_ -> class_ {methods = M.insert ident fun $ methods class_}

    extractFunction :: FunDef -> Except CheckError (Ident, Function)
    extractFunction (FunDef type_ ident args block) = do 
      types <- evalStateT (mapM extractArg args) S.empty
      return $ (ident, Fun type_ types)
      `catchError` \err -> throwError $ InFunctionError ident err 

    extractArg :: Arg -> StateT (S.Set Ident) (Except CheckError) Type
    extractArg (Ar type_ ident) = do
      env <- get
      when (S.member ident env) $ throwError $ DuplicateArgumentName ident
      when (type_ == Void) $ throwError $ VoidVariable ident 
      (modify $ S.insert ident) >> return type_

---------------------------------------------------------------------------------------------------

    typeCheckProgram :: Program -> Env -> Except CheckError ()
    typeCheckProgram (Prog topdefs) env = runReaderT (mapM_ typeCheckTopDef topdefs) env

    typeCheckTopDef :: TopDef -> ReaderT Env (Except CheckError) ()
    typeCheckTopDef (TopFunDef fundef) = typeCheckFunDef fundef
    typeCheckTopDef (ClassExtDef ident1 ident2 items) = typeCheckTopDef $ ClassDef ident1 items
    typeCheckTopDef (ClassDef ident items) = do
      env' <- getClassEnv $ Just ident
      local (\env -> env' {
        functions = M.union (functions env) $ functions env'
      }) $ mapM_ typeCheckClassItemDef items

    typeCheckClassItemDef :: ClassItemDef -> ReaderT Env (Except CheckError) ()
    typeCheckClassItemDef (AttrDef type_ ident) = return ()
    typeCheckClassItemDef (MethodDef fundef) = typeCheckFunDef fundef

    typeCheckFunDef :: FunDef -> ReaderT Env (Except CheckError) ()
    typeCheckFunDef (FunDef type_ ident args block) = do
      vars <- return $ M.fromList $ map (\(Ar type_ ident) -> (ident, Var 0 type_)) args
      env <- asks $ \env ->  env {variables = M.union (variables env) vars} 
      retStm <- lift $ runReaderT (execStateT (runContT (typeCheckBlock block) return) False) $ Ctxt 0 type_ env
      unless (type_ == Void || retStm) $ throwError $ NoReturnStatement
      `catchError` \err -> throwError $ InFunctionError ident err       

    --------------------------------------- Statements -------------------------------------
    
    typeCheckBlock :: Block -> ContT () (StateT Bool (ReaderT Context (Except CheckError))) ()
    typeCheckBlock (Blk stmts) = do
      lift $ local (\ctxt -> ctxt {level = level ctxt + 1}) $ runContT (mapM_ typeCheckStmt stmts) return
      ContT $ \next -> next ()

    typeCheckStmt :: Stmt -> ContT () (StateT Bool (ReaderT Context (Except CheckError))) ()
    typeCheckStmt (BStmt block) = typeCheckBlock block
    
    typeCheckStmt (Empty) = ContT $ \next -> next ()
    
    typeCheckStmt (Ret expr) = do
      env <- asks environment
      retType <- asks retType
      expType <- lift $ lift $ lift $ runReaderT (typeCheckExpr expr) env
      lift $ unless (retType == expType) $ throwError WrongTypes
      modify $ const True
      ContT $ \next -> next () 
    
    typeCheckStmt VRet = do
      retType <- asks retType
      lift $ unless (retType == Void) $ throwError $ VoidReturn
      ContT $ \next -> next ()
    
    typeCheckStmt (Ass lval expr) = do
      env <- asks environment
      lvalType <- lift $ lift $ lift $ runReaderT (typeCheckLVal lval) env
      expType <- lift $ lift $ lift $ runReaderT (typeCheckExpr expr) env
      lift $ unless (lvalType == expType) $ throwError WrongTypes
      ContT $ \next -> next ()

    typeCheckStmt (Decl type_ items) = do
      lvl <- asks level
      env <- asks environment
      env' <- lift $ lift $ lift $ runReaderT (execStateT (mapM_ typeCheckItem items) env) (type_, lvl)
      ContT $ \next -> local (\ctxt -> ctxt{environment = env'}) $ next ()

    typeCheckStmt (Cond expr stmt) = typeCheckStmt (CondElse expr stmt Empty)      
    typeCheckStmt (CondElse expr stmt1 stmt2) = do
      env <- asks environment
      expType <- lift $ lift $ lift $ runReaderT (typeCheckExpr expr) env
      lift $ unless (expType == Bool) $ throwError $ WrongTypes
      retStm1 <- lift $ lift $ execStateT (runContT (typeCheckStmt stmt1) return) False
      retStm2 <- lift $ lift $ execStateT (runContT (typeCheckStmt stmt2) return) False
      modify $ \ret -> ret || retStm1 && retStm2
      ContT $ \next -> next ()

    typeCheckStmt (While expr stmt) = do
      env <- asks environment
      expType <- lift $ lift $ lift $ runReaderT (typeCheckExpr expr) env
      lift $ unless (expType == Bool) $ throwError $ WrongTypes
      lift $ runContT (typeCheckStmt stmt) return
      ContT $ \next -> next ()

    typeCheckStmt (Decr lval) = typeCheckStmt (Incr lval)
    typeCheckStmt (Incr lval) = do
      env <- asks environment
      type_ <- lift $ lift $ lift $ runReaderT (typeCheckLVal lval) env
      lift $ unless (type_ == Int) $ throwError $ WrongTypes
      ContT $ \next -> next ()

    typeCheckStmt (For type_ ident expr stmt) = do
      lvl <- asks level
      env <- asks environment
      expType <- lift $ lift $ lift $ runReaderT (typeCheckExpr expr) env
      lift $ unless (expType == type_) $ throwError $ WrongTypes
      retStm <- lift $ lift $ execStateT (runContT (typeCheckStmt stmt) return) False
      modify $ \ret -> ret || retStm
      ContT $ \next -> next () 
    
    typeCheckStmt (SExp expr) = do
      env <- asks environment
      lift $ lift $ lift $ runReaderT (typeCheckExpr expr) env
      ContT $ \next -> next ()

    typeCheckItem :: Item -> StateT Env (ReaderT (Type, Integer) (Except CheckError)) ()
    typeCheckItem (Init ident expr) = do
      env <- get
      type_ <- asks fst
      expType <- lift $ lift $ runReaderT (typeCheckExpr expr) env
      unless (type_ == expType) $ throwError WrongTypes
      typeCheckItem (NoInit ident)

    typeCheckItem (NoInit ident) = do
      x <- get >>= \env -> return $ M.lookup ident $ variables env
      (type_, lvl) <- ask
      case x of
        Just var -> when (varDepth var == lvl) $ throwError DoubleDeclaration
        Nothing -> return ()
      modify $ \env -> env {variables = M.insert ident (Var lvl type_) $ variables env}
      
------------------------------------ Exprs ---------------------------------------------
    
    getClassEnv :: Maybe Ident -> ReaderT Env (Except CheckError) Env
    getClassEnv Nothing = return $ Env M.empty M.empty M.empty
    getClassEnv (Just ident) = do 
      class_ <- asks $ (M.! ident) . classes 
      env <- getClassEnv $ parent class_
      return env {
        variables = M.union (variables env) $ M.map (Var 0) (fields class_),
        functions = M.union (functions env) (methods class_)
      }

    typeCheckLVal :: LVal -> ReaderT Env (Except CheckError) Type
    typeCheckLVal (LVar ident) = do
      variables <- asks variables 
      unless (M.member ident variables) $ throwError WrongTypes
      return $ varType $ variables M.! ident

    -- Must be type ident []
    typeCheckLVal (LArr expr1 expr2) = do
      expType1 <- typeCheckExpr expr1
      expType2 <- typeCheckExpr expr2
      unless (expType2 == Int) $ throwError WrongTypes
      return expType2

    -- Error if expr type is not Obj (class)
    typeCheckLVal (LAttr expr ident) = do
      (Obj classIdent) <- typeCheckExpr expr
      variables <- fmap variables $ getClassEnv $ Just classIdent
      unless (M.member ident variables) $ throwError WrongTypes
      return $ varType $ variables M.! classIdent

    typeCheckExpr :: Expr -> ReaderT Env (Except CheckError) Type
    typeCheckExpr (ELitTrue) = return Bool
    
    typeCheckExpr (ELitFalse) = return Bool
    
    typeCheckExpr (EString string) = return Str
    
    typeCheckExpr (ELitInt integer) = return Int
    
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
      functions <- fmap functions $ getClassEnv $ Just classIdent
      unless (M.member ident functions) $ throwError WrongTypes
      return $ ret $ functions M.! classIdent  

    typeCheckExpr (ENewArr type_ expr) = do
      expType <- typeCheckExpr expr
      unless (expType == Int) $ throwError WrongTypes
      return type_

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
      unless (expType1 == Bool && expType2 == Bool) $ throwError WrongTypes
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