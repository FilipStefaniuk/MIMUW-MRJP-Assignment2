module Middleend where
    
    import AbsLatte
    import qualified AbsLLVM as LLVM
    import ErrM
    import Control.Monad.State
    import Control.Monad.Reader
    import Control.Monad.Writer
    import Control.Monad.Cont

    import qualified Data.Map as M

    -- data Variable = Var {
      -- varType :: LLVM.Type,
      -- varVal :: LLVM.LIdent
    -- }

    data Value = Val {
      valType :: LLVM.Type,
      val :: LLVM.Val
    }

    -- data Context = Ctxt {
      -- varCounter :: Integer,
      -- labelCounter :: Integer,
      -- blocks :: [LLVM.Block] -- Probably change that
    -- }

    -- data Env = Env {
      -- functions :: M.Map Ident 
      -- variables :: M.Map Ident Variable
    -- }

    -- getNextVarName :: StateT Context IO LLVM.LIdent
    -- getNextVarName = do
    --   nr <- gets varCounter
    --   modify $ \ctxt -> ctxt{varCounter = nr + 1}
    --   return $ LLVM.LIdent $ "%" ++ (show nr)


    -- getNextLabel :: StateT Context IO LLVM.Ident
    -- getNextLabel = do
    --   nr <- gets labelCounter
    --   modify $ \ctxt -> ctxt{labelCounter = nr + 1}
    --   return $ LLVM.Ident $ "L" ++ (show nr)

    -- globalIdent :: Ident -> LLVM.GIdent
    -- globalIdent (Ident str) = LLVM.GIdent $ "@" ++ str 

    -- builtInFunctions :: [LLVM.TopDef]
    -- builtInFunctions = [
    --   (LLVM.FunDecl LLVM.Void (globalIdent $ Ident ("printString")) [LLVM.Str]),
    --   (LLVM.FunDecl LLVM.Void (globalIdent $ Ident ("error")) []),
    --   (LLVM.FunDecl LLVM.Int (globalIdent $ Ident ("readInt")) []),
    --   (LLVM.FunDecl (LLVM.Ptr LLVM.Str) (globalIdent $ Ident ("readString")) []),
    --   (LLVM.FunDecl (LLVM.Ptr LLVM.Str) (globalIdent $ Ident ("_appendString")) []),
    --   (LLVM.FunDecl (LLVM.Ptr LLVM.Str) (globalIdent $ Ident ("malloc")) [])]

    transProgram :: Program -> IO LLVM.Program
    transProgram (Prog topdefs) = do
      topdefs <- mapM transTopDef topdefs
      return $ LLVM.Prog $ topdefs

    transTopDef :: TopDef -> IO LLVM.TopDef
    transTopDef (TopFunDef fundef) = transFunDef fundef
    
    -- * ClassDef ident classitemdefs -> failure x
    -- * ClassExtDef ident1 ident2 classitemdefs -> failure x

    -- * transClassItemDef :: ClassItemDef -> Result
    -- * transClassItemDef x = case x of
    -- * AttrDef type_ ident -> failure x
    -- * MethodDef fundef -> failure x

    transFunDef :: FunDef -> IO LLVM.TopDef
    transFunDef (FunDef type_ (Ident str) args block) = 
      (`evalStateT` 1) $ do
        args <- mapM collectArg args
        vars <- collectLocalVars block
        simpleBlocks <- evalStateT (execWriterT (runReaderT (transBlock block) (0, M.fromList (args ++ vars)))) (0, []) 
        let fstBlk = LLVM.FirstBlock (allocateLocalVars $ map snd (args ++ vars)) $ LLVM.StmBr $ LLVM.Ident $ "L" ++ (show 0) in
          return $ LLVM.FunDef (transType type_) (LLVM.Ident str) (map (uncurry LLVM.Arg . snd) args) fstBlk simpleBlocks

      -- ((args, env'), ctxt) <- lift $ runStateT (runStateT (mapM transArg args) env) $ Ctxt 1 0 []
      -- blks <- lift $ execStateT ((execWriterT (runReaderT (runContT (transBlock block) return) env)) >>= addFinalBlock) $ Ctxt 1 0 []

    collectArg :: Arg -> StateT Integer IO ((Ident, Integer), (LLVM.Type, LLVM.Ident))
    collectArg (Ar type_ ident) = do
      nr <- get
      modify (+1)
      return ((ident, 0), (transType type_, LLVM.Ident $ "%" ++ (show nr)))

    collectLocalVars :: Block -> StateT Integer IO [((Ident, Integer), (LLVM.Type, LLVM.Ident))]
    collectLocalVars block = do
      items <- lift $ runReaderT (execStateT (collectFromBlock block) []) 0
      localVars <- mapM makeLocalVar items
      return localVars

    makeLocalVar :: (Ident, Integer, Type) -> StateT Integer IO ((Ident, Integer),(LLVM.Type, LLVM.Ident))
    makeLocalVar (ident, lvl, type_) = do
      nr <- get
      modify (+1)
      return ((ident, lvl),(transType type_, LLVM.Ident $ "%" ++ (show nr)))

    allocateLocalVars :: [(LLVM.Type, LLVM.Ident)] -> [LLVM.DeclInstr]
    allocateLocalVars items = map (uncurry $ flip LLVM.Alloca) items

    collectFromBlock :: Block -> StateT [(Ident, Integer, Type)] (ReaderT Integer IO) ()
    collectFromBlock (Blk stmts) = local (+1) $ mapM_ collectFromStmt stmts

    collectFromStmt :: Stmt -> StateT [(Ident, Integer, Type)] (ReaderT Integer IO) ()
    collectFromStmt (BStmt block) = collectFromBlock block
    collectFromStmt (Decl type_ items) = mapM_ (collectFromItem type_) items
    collectFromStmt (While expr stmt) = local (+1) $ collectFromStmt stmt
    collectFromStmt (Cond expr stmt) = local (+1) $ collectFromStmt stmt
    collectFromStmt (CondElse expr stmt1 stmt2) = local (+1) $ (collectFromStmt stmt1) >> (collectFromStmt stmt2)
    collectFromStmt _ = return ()

    collectFromItem :: Type -> Item -> StateT [(Ident, Integer, Type)] (ReaderT Integer IO) ()
    collectFromItem type_ (NoInit ident) = do
      lvl <- ask
      modify (++[(ident, lvl, type_)]) 
    collectFromItem type_ (Init ident expr) = do
      lvl <- ask
      modify (++[(ident, lvl, type_)])

    -- transArg :: Arg -> StateT Integer IO LLVM.Arg
    -- transArg (Ar type_ ident) = do
    --   nr <- get
    --   modify (+1)
    --   return $ LLVM.Arg (transType type_) $ LLVM.Ident $ "%" ++ (show nr)

    -- addFinalBlock :: [LLVM.Instr] -> StateT Context IO ()
    -- addFinalBlock instr = do
    --   label <- getNextLabel
    --   modify $ \ctxt -> ctxt{blocks = (blocks ctxt) ++ [(LLVM.Blk label instr)]}

    ----------------------- Statements ------------------------------------------------------------
    
    transBlock :: Block -> ReaderT (Integer, M.Map (Ident, Integer) (LLVM.Type, LLVM.Ident)) (WriterT [LLVM.CodeBlock] (StateT (Integer, [LLVM.Instr]) (StateT Integer IO))) ()
    transBlock (Blk stmts) = local (\(lvl, m) -> (lvl+1, m)) $ mapM_ transStmt stmts

    transStmt :: Stmt -> ReaderT (Integer, M.Map (Ident, Integer) (LLVM.Type, LLVM.Ident)) (WriterT [LLVM.CodeBlock] (StateT (Integer, [LLVM.Instr]) (StateT Integer IO))) ()
    transStmt Empty = return ()
    transStmt (BStmt block) = transBlock block
    -- transStmt VRet = do 
    --   -- lift $ tell [LLVM.VoidRet]
    --   -- ContT $ \next -> next ()

    transStmt (Ret expr) = do
      (Val type_ val) <- transExpr expr
      (label, instrs) <- get
      nr <- lift $ lift $ lift $ get
      lift $ lift $ lift $ modify (+1)
      modify (const (nr, []))
      tell [LLVM.CodeBlock (LLVM.Ident $ "L" ++ (show label)) instrs $ LLVM.StmRet type_ val]

    transStmt (Decl type_ items) = mapM_ transItem items

    transStmt (Ass lval expr) = do
      (type1, iden) <- transLVal lval      
      (Val type2 val) <- transExpr expr
      modify $ \(label, instrs) -> (label, instrs ++ [LLVM.Store type2 val (LLVM.TPtr type1) iden])

    -- transStmt (Incr lval) = do
    --   name <- getNextVarName
    --   vars <- gets variables
    --   var <- return $ vars M.! (transLVal lval)
    --   modify $ \ctxt -> ctxt{variables = M.insert (transLVal lval) (fst var, LLVM.ValLVar name) $ variables ctxt}
    --   return $ [LLVM.OpBin name LLVM.OpAdd LLVM.Int (snd var) (LLVM.ValInt 1)]
    -- transStmt (SExp expr) = return []
      
    transStmt (CondElse expr stmt1 stmt2) = do
      (Val type_ (LLVM.ValVar ident)) <- transExpr expr
      (label1, instrs1) <- get
      nr1 <- lift $ lift $ lift $ get
      lift $ lift $ lift $ modify (+1)
      modify (const (nr1, []))
      (lvl, m) <- ask
      blocks1 <- lift $ lift $ execWriterT $ runReaderT (transStmt stmt1) (lvl + 1, m) 
      (label2, instrs2) <- get
      nr2 <- lift $ lift $ lift $ get
      lift $ lift $ lift $ modify (+1)
      modify (const (nr2, []))
      blocks2 <- lift $ lift $ execWriterT $ runReaderT (transStmt stmt2) (lvl + 1, m)
      (label3, instrs3) <- get
      nr3 <- lift $ lift $ lift $ get
      lift $ lift $ lift $ modify (+1)
      modify (const (nr3, []))
      tell [LLVM.CodeBlock (LLVM.Ident $ "L" ++ (show label1)) instrs1 (LLVM.StmCBr type_ ident (LLVM.Ident $ "L" ++ (show nr1)) (LLVM.Ident $ "L" ++ (show nr2)))]
      tell blocks1
      tell [LLVM.CodeBlock (LLVM.Ident $ "L" ++ (show label2)) instrs2 (LLVM.StmBr (LLVM.Ident $ "L" ++ (show nr3)))]
      tell blocks2
      tell [LLVM.CodeBlock (LLVM.Ident $ "L" ++ (show label3)) instrs3 (LLVM.StmBr (LLVM.Ident $ "L" ++ (show nr3)))]

    -- Decr lval -> failure x
    -- transStmt (Cond expr stmt) -> 
    --   While expr stmt -> failure x
    -- *  For type_ ident expr stmt -> failure x

    transItem :: Item -> ReaderT (Integer, M.Map (Ident, Integer) (LLVM.Type, LLVM.Ident)) (WriterT [LLVM.CodeBlock] (StateT (Integer, [LLVM.Instr]) (StateT Integer IO))) ()
    transItem (NoInit ident) = return ()
    transItem (Init ident expr) = do
      (lvl, m) <- ask
      (type1, iden) <- return $ m M.! (ident, lvl)
      (Val type2 val) <- transExpr expr
      modify $ \(label, instrs) -> (label, instrs ++ [LLVM.Store type2 val (LLVM.TPtr type1) iden])

    -------------------------  Expressions --------------------------------------------------------

    transLVal :: LVal -> ReaderT (Integer, M.Map (Ident, Integer) (LLVM.Type, LLVM.Ident)) (WriterT [LLVM.CodeBlock] (StateT (Integer, [LLVM.Instr]) (StateT Integer IO))) (LLVM.Type, LLVM.Ident)
    transLVal (LVar ident) = do
      (lvl, vars) <- ask
      return $ vars M.! (ident, lvl)
      

    -- *  LArr expr1 expr2 -> failure x
    -- *  LAttr expr ident -> failure x
    
    transExpr :: Expr -> ReaderT (Integer, M.Map (Ident, Integer) (LLVM.Type, LLVM.Ident)) (WriterT [LLVM.CodeBlock] (StateT (Integer, [LLVM.Instr]) (StateT Integer IO))) Value
    transExpr (ELitInt integer) = return $ Val LLVM.TInt $ LLVM.ValConst integer
    transExpr ELitTrue = return $ Val LLVM.TBool $ LLVM.ValConst 1
    transExpr ELitFalse = return $ Val LLVM.TBool $ LLVM.ValConst 0
    transExpr (EVar lval) = do 
      (type_, iden) <- transLVal lval
      nr <- lift $ lift $ lift $ get
      lift $ lift $ lift $ modify (+1)
      modify $ \(label, instrs) -> (label, instrs ++ [LLVM.Load (LLVM.Ident $ "%" ++ (show nr))type_ (LLVM.TPtr type_) iden])
      return $ Val type_ $ LLVM.ValVar (LLVM.Ident $ "%" ++ (show nr))
    
      -- transExpr (EString string) = return (LLVM.Str, LLVM.ValStr string)
    -- Neg expr -> failure x
    -- Not expr -> failure x

    -- *  Null -> failure x
    -- *  ECall ident exprs -> failure x
    -- *  EMetCall expr ident exprs -> failure x
    -- *  ENewObj ident -> failure x
    -- *  ENewArr type_ expr -> failure x

    transExpr (EAdd expr1 addop expr2) = do
      (Val type1 val1) <- transExpr expr1
      (Val type2 val2) <- transExpr expr2
      nr <- lift $ lift $ lift $ get
      lift $ lift $ lift $ modify (+1)
      modify $ \(label, instrs) -> (label, instrs ++ [LLVM.StmOpBin (LLVM.Ident $ "%" ++ (show nr)) (transAddOp addop) type1 val1 val2])
      return $ Val LLVM.TInt $ LLVM.ValVar (LLVM.Ident $ "%" ++ (show nr))

    -- transExpr (EMul expr1 mulop expr2) = do
    --   (Var type1 val1) <- transExpr expr1
    --   (Var type2 val2) <- transExpr expr2
    --   name <- lift $ lift $ getNextVarName
    --   tell [LLVM.OpBin name (transMulOp mulop) LLVM.TInt val1 val2]
    --   return $ Var LLVM.TInt $ LLVM.ValLVar name

    -- transExpr (EAnd expr1 expr2) = do
    --   (Var type1 val1) <- transExpr expr1
    --   (Var type2 val2) <- transExpr expr2
    --   name <- lift $ lift $ getNextVarName
    --   tell [LLVM.OpBin name LLVM.And LLVM.TBool val1 val2]
    --   return $ Var LLVM.TBool $ LLVM.ValLVar name

    -- transExpr (EOr expr1 expr2) = do
    --   (Var type1 val1) <- transExpr expr1
    --   (Var type2 val2) <- transExpr expr2
    --   name <- lift $ lift $ getNextVarName
    --   tell [LLVM.OpBin name LLVM.Or LLVM.TBool val1 val2]
    --   return $ Var LLVM.TBool $ LLVM.ValLVar name

    transExpr (ERel expr1 relop expr2) = do
      (Val type1 val1) <- transExpr expr1
      (Val type2 val2) <- transExpr expr2
      nr <- lift $ lift $ lift $ get
      lift $ lift $ lift $ modify (+1)
      modify $ \(label, instrs) -> (label, instrs ++ [LLVM.ICmp (LLVM.Ident $ "%" ++ (show nr)) (transRelOp relop) type1 val1 val2])
      return $ Val LLVM.TBool $ LLVM.ValVar (LLVM.Ident $ "%" ++ (show nr))
    
    transAddOp :: AddOp -> LLVM.Op
    transAddOp Plus = LLVM.Add
    transAddOp Minus = LLVM.Sub
    
    transMulOp :: MulOp -> LLVM.Op
    transMulOp Times = LLVM.Mul
    transMulOp Div = LLVM.SDiv
    transMulOp Mod = LLVM.SRem

    transRelOp :: RelOp -> LLVM.Cond
    transRelOp LTH = LLVM.Slt
    transRelOp LE = LLVM.Sle
    transRelOp GTH = LLVM.Sgt
    transRelOp GE = LLVM.Sge
    transRelOp EQU = LLVM.Eq
    transRelOp NE = LLVM.Ne 
    
    transType :: Type -> LLVM.Type
    transType Int = LLVM.TInt
    transType Bool = LLVM.TBool

    -- *  Arr type_ -> failure x
    -- *  Obj ident -> failure x