module EmitLLVM where

    import AbsLLVM
    import PrintLLVM
    import Data.List

    class (Show a) => Emit a where
        emit :: a -> String
        emit a = show a

    instance Emit Program where
        emit (Prog topdefs) = unlines $ map emit topdefs
        
    instance Emit TopDef where
        emit (FunDecl type_ (Ident str) types) = 
            "declare " ++ (emit type_) ++ " @" ++ str ++ " (" ++ (intercalate ", " $ map emit types) ++ ")" 
        
        emit (FunDef type_ (Ident str) args firstblock codeblocks) = 
            "\ndefine " ++ (emit type_) ++ " @" ++ str ++ " (" ++ (intercalate ", " $ map emit args) ++ ") {\n" ++ (emit firstblock) ++ (unlines $ map emit codeblocks) ++ "}" 

        emit (StrDecl (Ident str) type_ string) = "@" ++ str ++ " = private constant " ++ (emit type_) ++ " c\"" ++ string ++ "\""

    instance Emit Ident where
        emit (Ident str) = "%" ++ str

    instance Emit Type where
        emit TInt = "i32"
        emit TSm = "i8"
        emit TBool = "i1"
        emit TVoid = "void"
        emit (TArr int type_) = "[" ++ (show int) ++ " x " ++ (emit type_) ++ "]"
        emit (TPtr type_) = (emit type_) ++ "*" 

    instance Emit Arg where
        emit (Arg type_ ident) = (emit type_) ++ " " ++ (emit ident)

    instance Emit FirstBlock where
        emit (FirstBlock declinstrs instrs jmpinstr) = 
            (unlines $ map emit declinstrs) ++ (unlines $ map emit instrs) ++ (emit jmpinstr) ++ "\n"
    
    instance Emit CodeBlock where
        emit (CodeBlock (Ident str) instrs jmpinstr) =
            "\n; <label>:" ++ str ++ "\n" ++ (unlines $ map emit instrs) ++ (emit jmpinstr)
    
    instance Emit DeclInstr where
        emit (Alloca ident type_) = "  " ++ (emit ident) ++ " = alloca " ++ (emit type_)
    
    instance Emit JmpInstr where
        emit (StmBr ident) = "  br label " ++ (emit ident)
        emit (StmCBr type_ val ident1 ident2) = "  br " ++ (emit type_) ++ " " ++ (emit val) ++ ", label " ++ (emit ident1) ++ ", label " ++ (emit ident2) 
        emit StmVoidRet = "  ret void"
        emit (StmRet type_ val) = "  ret " ++ (emit type_) ++ " " ++ (emit val)

    instance Emit Instr where
      emit (StmOpBin ident op type_ val1 val2) = "  " ++ (emit ident) ++ " = " ++ (emit op) ++ " " ++ (emit type_) ++ " " ++ (emit val1) ++ ", " ++ (emit val2)
      emit (ICmp ident cond type_ val1 val2) = "  " ++ (emit ident) ++ " = icmp " ++ (emit cond) ++ " " ++ (emit type_) ++ " " ++ (emit val1) ++ ", " ++ (emit val2)
      emit (Call ident1 type_ (Ident str) items) = "  " ++ (emit ident1) ++ " = call " ++ (emit type_) ++ " @" ++ str ++ "(" ++ (intercalate ", " $ map emit items) ++ ")"
      emit (VoidCall (Ident str) items) = "  call void @" ++ str ++ "(" ++ (intercalate ", " $ map emit items) ++ ")" 
      emit (Load ident1 type_1 type_2 ident2) = "  " ++ (emit ident1) ++ " = load " ++ (emit type_1) ++ ", " ++ (emit type_2) ++ " " ++ (emit ident2)  
      emit (Store type_1 val type_2 ident) = "  store " ++ (emit type_1) ++ " " ++ (emit val) ++ ", " ++ (emit type_2) ++ " " ++ (emit ident)
      emit (Bitcast ident type1 (ValVar (Ident str)) type2) = "  " ++ (emit ident) ++ " = bitcast " ++ (emit type1) ++ " @" ++ str ++ " to " ++ (emit type2)

    instance Emit Val where
        emit (ValVar ident) = emit ident
        emit (ValConst int) = show int

    instance Emit Item where
        emit (Item type_ val) = (emit type_) ++ " " ++ (emit val)

    instance Emit Op where
        emit Add = "add"
        emit Sub = "sub"
        emit Mul = "mul"
        emit SDiv = "sdiv"
        emit SRem = "srem"
        emit And = "and"
        emit Or = "or"

    instance Emit Cond where
        emit Eq = "eq"
        emit Ne = "ne"
        emit Sgt = "sgt"
        emit Sge = "sge"
        emit Slt = "slt"
        emit Sle = "slq"
            
            