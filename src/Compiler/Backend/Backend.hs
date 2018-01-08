module Backend.Backend where
    
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
    wasRet :: Bool,
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

type GenLLVM = WriterT [LLVM.CodeBlock] (StateT Context (ReaderT Env (StateT GContext IO)))

nextId :: StateT Context (ReaderT Env (StateT GContext IO)) LLVM.Ident
nextId = do
    i <- gets counter
    modify $ \ctxt -> ctxt {counter = (+1) $ counter ctxt}
    return $ LLVM.Ident $ (show i)

newBlock :: GenLLVM ()
newBlock = do
    label <- lift $ nextId
    modify $ \ctxt -> ctxt {blockLabel = label, blockBody = []}

addInstr :: LLVM.Instr -> GenLLVM ()
addInstr instr = do
    modify $ \ctxt -> ctxt {blockBody = blockBody ctxt ++ [instr]}

transProgram :: Program (Maybe a)-> IO LLVM.Program
transProgram (Prog _ topdefs) = do
    funs <- execStateT (mapM_ collectTopDef topdefs) M.empty
    (decls, funs) <- runStateT (mapM transBuiltin builtins) funs
    (topdefs, gstate) <- runStateT (runReaderT (mapM transTopDef topdefs) $ Env M.empty funs) $ GCtxt 1 []
    return $ LLVM.Prog $ decls ++ (gDefs gstate) ++ topdefs

builtins :: [(Ident, Type (Maybe a),[Type (Maybe b)])]
builtins = [
    (Ident "printInt", Void Nothing, [Int Nothing]),
    (Ident "printString", Void Nothing, [Str Nothing]),
    (Ident "error", Void Nothing, []),
    (Ident "readInt", Int Nothing, []),
    (Ident "readString", Str Nothing, [])]

transBuiltin :: (Ident, Type (Maybe a),[Type (Maybe b)]) -> StateT FEnv IO LLVM.TopDef
transBuiltin (ident@(Ident str), type_, types) = do
    modify $ M.insert ident (transType type_, LLVM.Ident str)
    return $ LLVM.FunDecl (transType type_) (LLVM.Ident str) (map transType types) 

collectTopDef :: TopDef (Maybe a)-> StateT FEnv IO ()
collectTopDef (TopFunDef _ (FunDef _ type_ ident@(Ident str) _ _)) = modify $ M.insert ident (transType type_, LLVM.Ident str) 

transTopDef :: TopDef (Maybe a)-> ReaderT Env (StateT GContext IO) LLVM.TopDef
transTopDef (TopFunDef _ fundef) = transFunDef fundef

-- * ClassDef ident classitemdefs -> failure x
-- * ClassExtDef ident1 ident2 classitemdefs -> failure x

-- * transClassItemDef :: ClassItemDef -> Result
-- * transClassItemDef x = case x of
-- * AttrDef type_ ident -> failure x
-- * MethodDef fundef -> failure x

transFunDef :: FunDef (Maybe a) -> ReaderT Env (StateT GContext IO) LLVM.TopDef
transFunDef (FunDef _ type_ (Ident str) args block) = 
    (`evalStateT` (Ctxt 1 False [] (LLVM.Ident (show 0)) [])) $ do
    (argsLLVM, env') <- ask >>= runStateT (mapM transArg args)
    (first:blocks) <- local (const env') $ execWriterT $ runContT (mapM_ storeArg (zip args argsLLVM) >> transBlock block) collectFinalBlock
    allocs <- allocateLocalVars
    let (LLVM.CodeBlock _ body jmp) = first in 
        return $ cleanLLVM $ LLVM.FunDef (transType type_) (LLVM.Ident str) argsLLVM (LLVM.FirstBlock allocs body jmp) blocks 

transArg :: Arg (Maybe a)-> StateT Env (StateT Context (ReaderT Env (StateT GContext IO))) LLVM.Arg
transArg (Ar _ type_ ident@(Ident str)) = do
    name <- lift $ nextId
    lift $ modify $ \ctxt -> ctxt {localVars = localVars ctxt ++ [(transType type_, name)]}
    modify $ \env -> env {variables = M.insert ident (transType type_, name) $ variables env}
    return $ LLVM.Arg (transType type_) $ LLVM.Ident str 

storeArg :: (Arg (Maybe a), LLVM.Arg) -> ContT () GenLLVM ()
storeArg ((Ar _ _ ident1), (LLVM.Arg type_ ident2)) = lift $ do
    (_, ident) <- transLVal $ LVar Nothing ident1
    addInstr $ LLVM.Store type_ (LLVM.ValVar ident2) (LLVM.TPtr type_) ident


collectFinalBlock :: a-> GenLLVM a
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

transBlock :: Block (Maybe a)-> ContT () GenLLVM ()
transBlock (Blk _ stmts) = do
    mapM_ transStmt stmts

transStmt :: Stmt (Maybe a)-> ContT () GenLLVM ()
transStmt (Empty _) = return ()

transStmt (BStmt _ block) = transBlock block

transStmt (VRet _) = do 
    label <- lift $ gets blockLabel
    body <- lift $ gets blockBody
    lift $ tell [LLVM.CodeBlock label body LLVM.StmVoidRet]
    lift $ newBlock
    lift $ modify (\ctxt -> ctxt {wasRet = True})
    ContT $ \_ -> return ()

transStmt (Ret _ expr) = do
    (Val type_ val) <- lift $ transExpr expr
    label <- lift $ gets blockLabel
    body <- lift $ gets blockBody
    lift $ tell [LLVM.CodeBlock label body $ LLVM.StmRet type_ val]
    lift $ newBlock
    lift $ modify (\ctxt -> ctxt {wasRet = True})
    ContT $ \_ -> return ()

transStmt (Decl _ type_ items) = do 
    venv <- asks variables
    venv' <- lift $ runReaderT (execStateT (mapM_ transItem items) venv) type_
    ContT $ \next -> local (\env -> env{variables=venv'}) $ next ()

transStmt (Ass _ lval expr) = lift $ do
    (Val type1 val) <- transExpr expr
    (type2, ident) <- transLVal lval      
    addInstr $ LLVM.Store type1 val (LLVM.TPtr type2) ident 

transStmt (Incr _ lval) = transStmt $ Ass Nothing lval $ EAdd Nothing (EVar Nothing lval) (Plus Nothing) (ELitInt Nothing 1)
transStmt (Decr _ lval) = transStmt $ Ass Nothing lval $ EAdd Nothing (EVar Nothing lval) (Minus Nothing) (ELitInt Nothing 1)

transStmt (Cond _ expr stmt) = transStmt $ CondElse Nothing expr stmt (Empty Nothing)    
transStmt (CondElse _ expr stmt1 stmt2) = lift $ do
    (Val type_ val) <- transExpr expr
    label0 <- gets blockLabel
    body0 <- gets blockBody
    newBlock
    beginLabel1 <- gets blockLabel
    blocks1 <- lift $ execWriterT $ runContT (transStmt stmt1) return
    label1 <- gets blockLabel
    body1 <- gets blockBody
    wasRet1 <- gets wasRet
    modify (\ctxt -> ctxt {wasRet = False})
    newBlock
    beginLabel2 <- gets blockLabel
    blocks2 <- lift $ execWriterT $ runContT (transStmt stmt1) return
    label2 <- gets blockLabel
    body2 <- gets blockBody
    wasRet2 <- gets wasRet
    newBlock
    modify (\ctxt -> ctxt {wasRet = wasRet1 && wasRet2})
    beginLabel3 <- gets blockLabel
    tell [LLVM.CodeBlock label0 body0 (LLVM.StmCBr type_ val beginLabel1 beginLabel2)]
    tell blocks1
    unless (wasRet1 && wasRet2) $ tell [LLVM.CodeBlock label1 body1 (LLVM.StmBr beginLabel3)]
    tell blocks2
    unless (wasRet1 && wasRet2) $ tell [LLVM.CodeBlock label2 body2 (LLVM.StmBr beginLabel3)]
    
transStmt (While _ expr stmt) = lift $ do
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

transStmt (SExp _ expr) = lift $ void (transExpr expr)

-- * For type_ ident expr stmt -> failure x

transItem :: Item (Maybe a)-> StateT VEnv (ReaderT (Type (Maybe a)) GenLLVM) ()
transItem (NoInit _ ident) = do
    type_ <- ask
    transItem $ Init Nothing ident $ defaultValue $ type_

transItem (Init _ ident expr) = do
    (Val type1 val) <- lift $ lift $ transExpr expr
    type_ <- asks transType
    name <- lift $ lift $ lift $ nextId
    modify $ M.insert ident (type_, name)
    lift $ lift $ modify $ \ctxt -> ctxt{localVars = localVars ctxt ++ [(type_, name)]}
    lift $ lift $ addInstr $ LLVM.Store type1 val (LLVM.TPtr type_) name  

-------------------------  Expressions --------------------------------------------------------

transLVal :: LVal (Maybe a)-> GenLLVM (LLVM.Type, LLVM.Ident)
transLVal (LVar _ ident) = asks $ (M.! ident) . variables

-- *  LArr expr1 expr2 -> failure x
-- *  LAttr expr ident -> failure x

transExpr :: Expr (Maybe a)-> GenLLVM Value
transExpr (ELitInt _ integer) = return $ Val LLVM.TInt $ LLVM.ValConst integer
transExpr (ELitTrue _ ) = return $ Val LLVM.TBool $ LLVM.ValConst 1
transExpr (ELitFalse _) = return $ Val LLVM.TBool $ LLVM.ValConst 0
transExpr (EVar _ lval) = do 
    (type_, ident) <- transLVal lval
    name <- lift $ nextId
    addInstr $ LLVM.Load name type_ (LLVM.TPtr type_) ident 
    return $ Val type_ $ LLVM.ValVar name

transExpr (Neg _ expr) = transExpr $ EMul Nothing expr (Times Nothing) (ELitInt Nothing $ -1)

transExpr (ECall _ ident exprs) = do
    exprs <- mapM transExpr exprs
    name <- lift $ nextId
    (type_, ident) <- asks $ (M.! ident) . functions
    case type_ of
        LLVM.TVoid -> addInstr $ LLVM.VoidCall ident (map (\(Val type_ val) -> LLVM.Item type_ val) exprs)
        _ -> addInstr $ LLVM.Call name type_ ident (map (\(Val type_ val) -> LLVM.Item type_ val) exprs)
    return $ Val type_ $ LLVM.ValVar name

transExpr (EString _ string) = let string' = tail $ init $ string in do
    name <- lift $ nextId
    gId <- lift $ lift $ gets gCounter
    lift $ lift $ modify $ \ctxt -> ctxt {gCounter = gCounter ctxt + 1, gDefs = gDefs ctxt ++ [LLVM.StrDecl (LLVM.Ident $ "str." ++ (show gId)) (LLVM.TArr (toInteger $ length string' + 1) LLVM.TSm) (string' ++ "\\00")]}
    addInstr $ LLVM.Bitcast name (LLVM.TPtr $LLVM.TArr (toInteger $ length string' + 1) LLVM.TSm) (LLVM.ValVar $ LLVM.Ident $ "str." ++ (show gId)) (LLVM.TPtr LLVM.TSm)
    return $ Val (LLVM.TPtr LLVM.TSm) $ LLVM.ValVar name

transExpr (Not _ expr) = do
    (Val type_ val) <- transExpr expr
    name <- lift $ nextId
    addInstr $ LLVM.ICmp name LLVM.Eq type_ val (LLVM.ValConst 0)
    return $ Val type_ $ LLVM.ValVar name

-- *  Null -> failure x
-- *  EMetCall expr ident exprs -> failure x
-- *  ENewObj ident -> failure x
-- *  ENewArr type_ expr -> failure x

transExpr (EAdd _ expr1 addop expr2) = do
    (Val type1 val1) <- transExpr expr1
    (Val type2 val2) <- transExpr expr2
    name <- lift $ nextId
    addInstr $ LLVM.StmOpBin name (transAddOp addop) type1 val1 val2
    return $ Val LLVM.TInt $ LLVM.ValVar name

transExpr (EMul _ expr1 mulop expr2) = do
    (Val type1 val1) <- transExpr expr1
    (Val type2 val2) <- transExpr expr2
    name <- lift $ nextId
    addInstr $ LLVM.StmOpBin name (transMulOp mulop) type1 val1 val2
    return $ Val LLVM.TInt $ LLVM.ValVar name

transExpr (EAnd _ expr1 expr2) = do
    (Val type1 val1) <- transExpr expr1
    (Val type2 val2) <- transExpr expr2
    name <- lift $ nextId
    addInstr $ LLVM.StmOpBin name LLVM.And LLVM.TBool val1 val2
    return $ Val LLVM.TBool $ LLVM.ValVar name

transExpr (EOr _ expr1 expr2) = do
    (Val type1 val1) <- transExpr expr1
    (Val type2 val2) <- transExpr expr2
    name <- lift $ nextId
    addInstr $ LLVM.StmOpBin name LLVM.Or LLVM.TBool val1 val2
    return $ Val LLVM.TBool $ LLVM.ValVar name

transExpr (ERel _ expr1 relop expr2) = do
    (Val type1 val1) <- transExpr expr1
    (Val type2 val2) <- transExpr expr2
    name <- lift $ nextId
    addInstr $ LLVM.ICmp name (transRelOp relop) type1 val1 val2
    return $ Val LLVM.TBool $ LLVM.ValVar name

transAddOp :: AddOp (Maybe a)-> LLVM.Op
transAddOp (Plus _) = LLVM.Add
transAddOp (Minus _) = LLVM.Sub

transMulOp :: MulOp (Maybe a)-> LLVM.Op
transMulOp (Times _) = LLVM.Mul
transMulOp (Div _) = LLVM.SDiv
transMulOp (Mod _) = LLVM.SRem

transRelOp :: RelOp (Maybe a)-> LLVM.Cond
transRelOp (LTH _) = LLVM.Slt
transRelOp (LE _) = LLVM.Sle
transRelOp (GTH _) = LLVM.Sgt
transRelOp (GE _) = LLVM.Sge
transRelOp (EQU _) = LLVM.Eq
transRelOp (NE _) = LLVM.Ne 

transType :: Type (Maybe a)-> LLVM.Type
transType (Int _) = LLVM.TInt
transType (Bool _) = LLVM.TBool
transType (Void _ )= LLVM.TVoid
transType (Str _) = LLVM.TPtr $ LLVM.TSm

defaultValue :: Type (Maybe a)-> Expr (Maybe a)
defaultValue (Int _)= ELitInt Nothing 0
defaultValue (Bool _) = ELitFalse Nothing
defaultValue (Str _) = EString Nothing ""

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
    val <- cleanVal val
    ident1 <- cleanIdent ident1
    ident2 <- cleanIdent ident2
    return $ LLVM.StmCBr type_ val ident1 ident2
cleanJmp (LLVM.StmRet type_ val) = do
    val <- cleanVal val
    return $ LLVM.StmRet type_ val
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