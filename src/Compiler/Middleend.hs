module Middleend where
    
    import AbsLatte
    import qualified AbsLLVM as LLVM
    import ErrM
    import Control.Monad.State
    import Control.Monad.Reader
    import Control.Monad.Writer
    import Control.Monad.Cont

    import qualified Data.Map as M

    data Value = Val {
      valType :: LLVM.Type,
      val :: LLVM.Val
    }

    data GContext = GCtxt {
      gCounter :: Integer,
      gDefs :: [LLVM.TopDef]
    }

    data Context = Ctxt {
      counter :: Integer,
      localVars :: [(LLVM.Type, LLVM.Ident)],
      blockLabel :: LLVM.Ident,
      blockBody :: [LLVM.Instr] 
    }

    type VEnv = M.Map Ident (LLVM.Type, LLVM.Ident)
    type FEnv = M.Map Ident (LLVM.Type, LLVM.Ident)

    data Env = Env {
      variables :: VEnv,
      functions :: FEnv
    }

    type GenIR = WriterT [LLVM.CodeBlock] (StateT Context (ReaderT Env (StateT GContext IO)))

    nextId :: StateT Context (ReaderT Env (StateT GContext IO)) LLVM.Ident
    nextId = do
      i <- gets counter
      modify $ \ctxt -> ctxt {counter = (+1) $ counter ctxt}
      return $ LLVM.Ident $ (show i)

    newBlock :: GenIR ()
    newBlock = do
      label <- lift $ nextId
      modify $ \ctxt -> ctxt {blockLabel = label, blockBody = []}

    addInstr :: LLVM.Instr -> GenIR ()
    addInstr instr = do
      modify $ \ctxt -> ctxt {blockBody = blockBody ctxt ++ [instr]}

    transProgram :: Program -> IO LLVM.Program
    transProgram (Prog topdefs) = do
      funs <- execStateT (mapM_ collectTopDef topdefs) M.empty
      (decls, funs) <- runStateT (mapM transBuiltin builtins) funs
      (topdefs, gstate) <- runStateT (runReaderT (mapM transTopDef topdefs) $ Env M.empty funs) $ GCtxt 1 []
      return $ LLVM.Prog $ decls ++ (gDefs gstate) ++ topdefs
    
    builtins :: [(Ident, Type, [Type])]
    builtins = [
      (Ident "printInt", Void, [Int]),
      (Ident "printString", Void, [Str]),
      (Ident "error", Void, []),
      (Ident "readInt", Int, []),
      (Ident "readString", Str, [])]

    transBuiltin :: (Ident, Type, [Type]) -> StateT FEnv IO LLVM.TopDef
    transBuiltin (ident@(Ident str), type_, types) = do
      modify $ M.insert ident (transType type_, LLVM.Ident str)
      return $ LLVM.FunDecl (transType type_) (LLVM.Ident str) (map transType types) 

    collectTopDef :: TopDef -> StateT FEnv IO ()
    collectTopDef (TopFunDef (FunDef type_ ident@(Ident str) _ _)) = modify $ M.insert ident (transType type_, LLVM.Ident str) 

    transTopDef :: TopDef -> ReaderT Env (StateT GContext IO) LLVM.TopDef
    transTopDef (TopFunDef fundef) = transFunDef fundef
    
    -- * ClassDef ident classitemdefs -> failure x
    -- * ClassExtDef ident1 ident2 classitemdefs -> failure x

    -- * transClassItemDef :: ClassItemDef -> Result
    -- * transClassItemDef x = case x of
    -- * AttrDef type_ ident -> failure x
    -- * MethodDef fundef -> failure x

    transFunDef :: FunDef -> ReaderT Env (StateT GContext IO) LLVM.TopDef
    transFunDef (FunDef type_ (Ident str) args block) = 
      (`evalStateT` (Ctxt 1 [] (LLVM.Ident (show 0)) [])) $ do
        (argsLLVM, env') <- ask >>= runStateT (mapM transArg args)
        (first:blocks) <- local (const env') $ execWriterT $ runContT (mapM_ storeArg (zip args argsLLVM) >> transBlock block) collectFinalBlock
        allocs <- allocateLocalVars
        let (LLVM.CodeBlock _ body jmp) = first in 
          return $ cleanLLVM $ LLVM.FunDef (transType type_) (LLVM.Ident str) argsLLVM (LLVM.FirstBlock allocs body jmp) blocks 

    transArg :: Arg -> StateT Env (StateT Context (ReaderT Env (StateT GContext IO))) LLVM.Arg
    transArg (Ar type_ ident@(Ident str)) = do
      name <- lift $ nextId
      lift $ modify $ \ctxt -> ctxt {localVars = localVars ctxt ++ [(transType type_, name)]}
      modify $ \env -> env {variables = M.insert ident (transType type_, name) $ variables env}
      return $ LLVM.Arg (transType type_) $ LLVM.Ident str 

    storeArg :: (Arg, LLVM.Arg) -> ContT () GenIR ()
    storeArg ((Ar _ ident1), (LLVM.Arg type_ ident2)) = lift $ do
      (_, ident) <- transLVal $ LVar ident1
      addInstr $ LLVM.Store type_ (LLVM.ValVar ident2) (LLVM.TPtr type_) ident


    collectFinalBlock :: a -> GenIR a
    collectFinalBlock x = do
      label <- gets blockLabel
      body <- gets blockBody
      if null body then return x 
      else tell [LLVM.CodeBlock label body LLVM.StmVoidRet] >> return x

    allocateLocalVars :: StateT Context (ReaderT Env (StateT GContext IO)) [LLVM.DeclInstr]
    allocateLocalVars = do
      localVars <- gets localVars
      return $ map (uncurry $ flip LLVM.Alloca) localVars

    ----------------------- Statements ------------------------------------------------------------
    
    transBlock :: Block -> ContT () GenIR ()
    transBlock (Blk stmts) = do
      mapM_ transStmt stmts

    transStmt :: Stmt -> ContT () GenIR ()
    transStmt Empty = return ()
    
    transStmt (BStmt block) = transBlock block
    
    transStmt VRet = lift $ do 
      label <- gets blockLabel
      body <- gets blockBody
      tell [LLVM.CodeBlock label body LLVM.StmVoidRet]
      newBlock
      
    transStmt (Ret expr) = lift $ do
      (Val type_ val) <- transExpr expr
      label <- gets blockLabel
      body <- gets blockBody
      tell [LLVM.CodeBlock label body $ LLVM.StmRet type_ val]
      newBlock

    transStmt (Decl type_ items) = do 
      venv <- asks variables
      venv' <- lift $ runReaderT (execStateT (mapM_ transItem items) venv) type_
      ContT $ \next -> local (\env -> env{variables=venv'}) $ next ()

    transStmt (Ass lval expr) = lift $ do
      (Val type1 val) <- transExpr expr
      (type2, ident) <- transLVal lval      
      addInstr $ LLVM.Store type1 val (LLVM.TPtr type2) ident 

    transStmt (Incr lval) = transStmt $ Ass lval $ EAdd (EVar lval) Plus (ELitInt 1)
    transStmt (Decr lval) = transStmt $ Ass lval $ EAdd (EVar lval) Minus (ELitInt 1)

    transStmt (Cond expr stmt) = transStmt $ CondElse expr stmt Empty    
    transStmt (CondElse expr stmt1 stmt2) = lift $ do
      (Val type_ val) <- transExpr expr
      label0 <- gets blockLabel
      body0 <- gets blockBody
      newBlock
      beginLabel1 <- gets blockLabel
      blocks1 <- lift $ execWriterT $ runContT (transStmt stmt1) return
      label1 <- gets blockLabel
      body1 <- gets blockBody
      newBlock
      beginLabel2 <- gets blockLabel
      blocks2 <- lift $ execWriterT $ runContT (transStmt stmt1) return
      label2 <- gets blockLabel
      body2 <- gets blockBody
      newBlock
      beginLabel3 <- gets blockLabel
      tell [LLVM.CodeBlock label0 body0 (LLVM.StmCBr type_ val beginLabel1 beginLabel2)]
      tell blocks1
      tell [LLVM.CodeBlock label1 body1 (LLVM.StmBr beginLabel3)]
      tell blocks2
      tell [LLVM.CodeBlock label2 body2 (LLVM.StmBr beginLabel3)]
      
    transStmt (While expr stmt) = lift $ do
      label0 <- gets blockLabel
      body0 <- gets blockBody
      newBlock
      (Val type_ val) <- transExpr expr
      label1 <- gets blockLabel
      body1 <- gets blockBody
      newBlock
      beginLabel <- gets blockLabel
      blocks <- lift $ execWriterT $ runContT (transStmt stmt) return
      label2 <- gets blockLabel
      body2 <- gets blockBody
      newBlock
      endLabel <- gets blockLabel
      tell [LLVM.CodeBlock label0 body0 (LLVM.StmBr label1)]
      tell [LLVM.CodeBlock label1 body1 (LLVM.StmCBr type_ val beginLabel endLabel)]
      tell blocks
      tell [LLVM.CodeBlock label2 body2 (LLVM.StmBr label1)]

    transStmt (SExp expr) = lift $ transExpr expr >> return ()

    -- * For type_ ident expr stmt -> failure x

    transItem :: Item -> StateT VEnv (ReaderT Type GenIR) ()
    transItem (NoInit ident) = do
      type_ <- ask
      transItem $ Init ident $ defaultValue type_

    transItem (Init ident expr) = do
      (Val type1 val) <- lift $ lift $ transExpr expr
      type_ <- asks transType
      name <- lift $ lift $ lift $ nextId
      modify $ M.insert ident (type_, name)
      lift $ lift $ modify $ \ctxt -> ctxt{localVars = localVars ctxt ++ [(type_, name)]}
      lift $ lift $ addInstr $ LLVM.Store type1 val (LLVM.TPtr type_) name  

    -------------------------  Expressions --------------------------------------------------------

    transLVal :: LVal -> GenIR (LLVM.Type, LLVM.Ident)
    transLVal (LVar ident) = asks $ (M.! ident) . variables
  
    -- *  LArr expr1 expr2 -> failure x
    -- *  LAttr expr ident -> failure x
    
    transExpr :: Expr -> GenIR Value
    transExpr (ELitInt integer) = return $ Val LLVM.TInt $ LLVM.ValConst integer
    transExpr ELitTrue = return $ Val LLVM.TBool $ LLVM.ValConst 1
    transExpr ELitFalse = return $ Val LLVM.TBool $ LLVM.ValConst 0
    transExpr (EVar lval) = do 
      (type_, ident) <- transLVal lval
      name <- lift $ nextId
      addInstr $ LLVM.Load name type_ (LLVM.TPtr type_) ident 
      return $ Val type_ $ LLVM.ValVar name
    
    transExpr (Neg expr) = transExpr $ EMul expr Times (ELitInt $ -1)

    transExpr (ECall ident exprs) = do
      exprs <- mapM transExpr exprs
      name <- lift $ nextId
      (type_, ident) <- asks $ (M.! ident) . functions
      case type_ of
        LLVM.TVoid -> addInstr $ LLVM.VoidCall ident (map (\(Val type_ val) -> LLVM.Item type_ val) exprs)
        _ -> addInstr $ LLVM.Call name type_ ident (map (\(Val type_ val) -> LLVM.Item type_ val) exprs)
      return $ Val type_ $ LLVM.ValVar name

    transExpr (EString string) = do
      name <- lift $ nextId
      gId <- lift $ lift $ gets gCounter
      lift $ lift $ modify $ \ctxt -> ctxt {gCounter = gCounter ctxt + 1, gDefs = gDefs ctxt ++ [LLVM.StrDecl (LLVM.Ident $ "str." ++ (show gId)) (LLVM.TArr (toInteger $ length string) LLVM.TSm) string]}
      addInstr $ LLVM.Bitcast name (LLVM.TPtr $LLVM.TArr (toInteger $ length string) LLVM.TSm) (LLVM.ValVar $ LLVM.Ident $ "str." ++ (show gId)) (LLVM.TPtr LLVM.TSm)
      return $ Val (LLVM.TPtr LLVM.TSm) $ LLVM.ValVar name
    
    -- Not expr -> failure x
    
    -- *  Null -> failure x
    -- *  EMetCall expr ident exprs -> failure x
    -- *  ENewObj ident -> failure x
    -- *  ENewArr type_ expr -> failure x

    transExpr (EAdd expr1 addop expr2) = do
      (Val type1 val1) <- transExpr expr1
      (Val type2 val2) <- transExpr expr2
      name <- lift $ nextId
      addInstr $ LLVM.StmOpBin name (transAddOp addop) type1 val1 val2
      return $ Val LLVM.TInt $ LLVM.ValVar name

    transExpr (EMul expr1 mulop expr2) = do
      (Val type1 val1) <- transExpr expr1
      (Val type2 val2) <- transExpr expr2
      name <- lift $ nextId
      addInstr $ LLVM.StmOpBin name (transMulOp mulop) type1 val1 val2
      return $ Val LLVM.TInt $ LLVM.ValVar name

    transExpr (EAnd expr1 expr2) = do
      (Val type1 val1) <- transExpr expr1
      (Val type2 val2) <- transExpr expr2
      name <- lift $ nextId
      addInstr $ LLVM.StmOpBin name LLVM.And LLVM.TBool val1 val2
      return $ Val LLVM.TBool $ LLVM.ValVar name

    transExpr (EOr expr1 expr2) = do
      (Val type1 val1) <- transExpr expr1
      (Val type2 val2) <- transExpr expr2
      name <- lift $ nextId
      addInstr $ LLVM.StmOpBin name LLVM.Or LLVM.TBool val1 val2
      return $ Val LLVM.TBool $ LLVM.ValVar name

    transExpr (ERel expr1 relop expr2) = do
      (Val type1 val1) <- transExpr expr1
      (Val type2 val2) <- transExpr expr2
      name <- lift $ nextId
      addInstr $ LLVM.ICmp name (transRelOp relop) type1 val1 val2
      return $ Val LLVM.TBool $ LLVM.ValVar name
    
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
    transType Void = LLVM.TVoid
    transType Str = LLVM.TPtr $ LLVM.TSm

    defaultValue :: Type -> Expr
    defaultValue Int = ELitInt 0
    defaultValue Bool = ELitFalse
    defaultValue Str = EString ""

    -- *  Arr type_ -> failure x
    -- *  Obj ident -> failure x


---------------------------------------------------------------------------------------------------

    cleanLLVM :: LLVM.TopDef -> LLVM.TopDef
    cleanLLVM f@(LLVM.FunDef ident type_ args fblock blocks) = 
      (`evalState` (1, M.empty)) $ do
        mapM_ cleanArg args
        fblock <- cleanFirstBlock fblock
        blocks <- mapM cleanBlock blocks
        fblock <- cleanJmpFirstBlock fblock
        blocks <- mapM cleanJmpBlock blocks
        return $ LLVM.FunDef ident type_ args fblock blocks

    cleanArg :: LLVM.Arg -> State (Integer, M.Map LLVM.Ident LLVM.Ident) ()
    cleanArg (LLVM.Arg type_ ident) = do
      modify $ \(i, m) -> (i, M.insert ident ident m)

    cleanFirstBlock :: LLVM.FirstBlock -> State (Integer, M.Map LLVM.Ident LLVM.Ident) LLVM.FirstBlock
    cleanFirstBlock (LLVM.FirstBlock decls body jmp) = do
      decls <- mapM cleanDecl decls
      body <- mapM cleanInstr body
      return $ LLVM.FirstBlock decls body jmp

    cleanJmpFirstBlock :: LLVM.FirstBlock -> State (Integer, M.Map LLVM.Ident LLVM.Ident) LLVM.FirstBlock
    cleanJmpFirstBlock (LLVM.FirstBlock decls body jmp) = do
      jmp <- cleanJmp jmp
      return $ LLVM.FirstBlock decls body jmp

    cleanBlock :: LLVM.CodeBlock -> State (Integer, M.Map LLVM.Ident LLVM.Ident) LLVM.CodeBlock
    cleanBlock (LLVM.CodeBlock label body jmp) = do
      label <- cleanIdent label
      body <- mapM cleanInstr body
      return $ LLVM.CodeBlock label body jmp

    cleanJmpBlock :: LLVM.CodeBlock -> State (Integer, M.Map LLVM.Ident LLVM.Ident) LLVM.CodeBlock
    cleanJmpBlock (LLVM.CodeBlock label body jmp) = do
      jmp <- cleanJmp jmp
      return $ LLVM.CodeBlock label body jmp

    cleanDecl :: LLVM.DeclInstr -> State (Integer, M.Map LLVM.Ident LLVM.Ident) LLVM.DeclInstr
    cleanDecl (LLVM.Alloca ident type_) = do
      ident <- cleanIdent ident
      return $ LLVM.Alloca ident type_

    cleanInstr :: LLVM.Instr -> State (Integer, M.Map LLVM.Ident LLVM.Ident) LLVM.Instr
    cleanInstr (LLVM.StmOpBin ident op type_ val1 val2) = do
      ident <- cleanIdent ident
      val1 <- cleanVal val1
      val2 <- cleanVal val2
      return $ LLVM.StmOpBin ident op type_ val1 val2

    cleanInstr (LLVM.ICmp ident cond type_ val1 val2) = do
      ident <- cleanIdent ident
      val1 <- cleanVal val1
      val2 <- cleanVal val2
      return $ LLVM.ICmp ident cond type_ val1 val2

    cleanInstr (LLVM.Call ident1 type_ ident2 items) = do
      ident1 <- cleanIdent ident1
      items <- mapM cleanItem items
      return $ LLVM.Call ident1 type_ ident2 items

    cleanInstr (LLVM.VoidCall ident items) = do
      items <- mapM cleanItem items
      return $ LLVM.VoidCall ident items

    cleanInstr (LLVM.Load ident1 type_1 type_2 ident2) = do
      ident1 <- cleanIdent ident1
      ident2 <- cleanIdent ident2
      return $ LLVM.Load ident1 type_1 type_2 ident2
    
    cleanInstr (LLVM.Store type_1 val type_2 ident) = do
      val <- cleanVal val
      ident <- cleanIdent ident
      return $ LLVM.Store type_1 val type_2 ident

    cleanInstr (LLVM.Bitcast ident type1 val type2) = do
      ident <- cleanIdent ident
      return $ LLVM.Bitcast ident type1 val type2

    cleanJmp :: LLVM.JmpInstr -> State (Integer, M.Map LLVM.Ident LLVM.Ident) LLVM.JmpInstr
    cleanJmp (LLVM.StmBr ident) = do
      ident <- cleanIdent ident
      return $ LLVM.StmBr ident
    cleanJmp (LLVM.StmCBr type_ val ident1 ident2) = do
      ident1 <- cleanIdent ident1
      ident2 <- cleanIdent ident2
      return $ LLVM.StmCBr type_ val ident1 ident2
    cleanJmp x = return x

    cleanVal :: LLVM.Val -> State (Integer, M.Map LLVM.Ident LLVM.Ident) LLVM.Val
    cleanVal (LLVM.ValVar ident) = do
      ident <- cleanIdent ident
      return $ LLVM.ValVar ident
    cleanVal x = return x

    cleanItem :: LLVM.Item -> State (Integer, M.Map LLVM.Ident LLVM.Ident) LLVM.Item
    cleanItem (LLVM.Item type_ val) = do
      val <- cleanVal val
      return $ LLVM.Item type_ val

    cleanIdent :: LLVM.Ident -> State (Integer, M.Map LLVM.Ident LLVM.Ident) LLVM.Ident
    cleanIdent ident = do
      idents <- gets snd
      case M.lookup ident idents of
        (Just ident) -> return $ ident
        Nothing -> do
          newIdent <- gets fst
          modify $ \(i, m) -> (i+1, M.insert ident (LLVM.Ident (show newIdent)) m)
          return $ LLVM.Ident (show newIdent)  