module Frontend.Printer where

import Frontend.LLVM
import Data.List

printProgram :: Program -> String
printProgram (Program classDefs stringDefs funcDefs) = unlines . intercalate [""] $ [
    printBuiltInFunctions,
    map prt classDefs,
    map prt stringDefs,
    (intercalate [""]) . map printFuncDef $ funcDefs
    ]

printFuncDef :: FunctionDef -> [String]
printFuncDef (FunctionDef ty ident args allocs blocks) = concat ([
    [concat ["define ", prt ty, " ", prt ident, "(", intercalate ", " (map (\(ty,ident) -> (prt ty) ++ " " ++ (prt ident)) args),") {"]],
    map (("  " ++) . prt) allocs,
    intercalate [""] . map printBlock $ blocks,
    ["}"]])

printBlock :: CodeBlock -> [String]
printBlock (CodeBlock (LocalIdent str) phi body end) = 
    let codeblock = map ("  " ++ ) $ concat [map prt phi, map prt body, [prt end]] in
        if str == "0" then codeblock
        else ("; <label>:" ++ str):codeblock

printBuiltInFunctions :: [String]
printBuiltInFunctions = [
    "declare void @printInt(i32)",
    "declare void @printString(i8*)",
    "declare void @error()",
    "declare i32 @readInt()",
    "declare i8* @readString()",
    "declare i8* @concat(i8*, i8*)",
    "declare i8* @malloc(i32)",
    "declare void @llvm.memset.p0i8.i32(i8*, i8, i32, i32, i1)"]

class Printer a where
    prt :: a -> String
    
instance Printer ClassDef where
    prt (ClassDef ident tys) = unwords [prt ident, "=", "type", "{", intercalate ", " $ map prt tys, "}"]

instance Printer StringDef where
    prt (StringDef ident ty str) = unwords [prt ident, "=", "constant", prt ty, "c\"" ++ str ++ "\\00\""]

instance Printer Alloc where
    prt (Alloc ident ty) = unwords [prt ident, "=", "alloca", prt ty]

instance Printer Phi where
    prt (Phi ident ty branches) = unwords [prt ident, "=", "phi", prt ty, intercalate ", " $ map prt branches]

instance Printer PhiBranch where
    prt (PhiBranch ident op) = unwords ["[", prt op ++ ",", prt ident, "]"]

instance Printer BlockEnd where
    prt (BlockEndBranch ident) = unwords ["br", "label", prt ident]
    prt (BlockEndBranchCond ty op ident1 ident2) = unwords ["br", prt ty, prt op ++ ",", "label", prt ident1 ++ ",", "label", prt ident2]
    prt (BlockEndReturn ty op) = unwords ["ret", prt ty, prt op]
    prt (BlockEndReturnVoid) = unwords ["ret", "void"]

instance Printer Instruction where
    prt (InstrBinOp res binOp ty op1 op2) = unwords [prt res, "=", prt binOp, prt ty, prt op1 ++ ",", prt op2]
    prt (InstrCall res ty ident args) = unwords [prt res, "=", "call", prt ty, prt ident ++ "(" ++ (intercalate ", " $ map (\(ty, op)-> (prt ty) ++ " " ++ (prt op)) args) ++ ")"]
    prt (InstrVoidCall ident args) = unwords ["call", "void", prt ident ++ "(" ++ (intercalate ", " $ map (\(ty, op)-> (prt ty) ++ " " ++ (prt op)) args) ++ ")"]
    prt (InstrGetElementPtr res ty1 ty2 op idxs) = unwords [prt res, "=", "getelementptr", prt ty1 ++ ",", prt ty2, prt op ++ ",", intercalate ", " $ map (\op -> "i32 " ++ (prt op)) idxs]
    prt (InstrCmp res cond ty op1 op2) = unwords [prt res, "=", "icmp", prt cond, prt ty, prt op1 ++ ",", prt op2]
    prt (InstrBitcast res ty1 op ty2) = unwords [prt res, "=", "bitcast", prt ty1, prt op, "to", prt ty2]
    prt (InstrTrunc res ty1 op ty2) = unwords [prt res, "=", "trunc", prt ty1, prt op, "to", prt ty2]
    prt (InstrZext res ty1 op ty2) = unwords [prt res, "=", "zext", prt ty1, prt op, "to", prt ty2]
    prt (InstrLoad res ty1 ty2 op) = unwords [prt res, "=", "load", prt ty1 ++ ",", prt ty2, prt op]
    prt (InstrStore ty1 op1 ty2 op2) = unwords ["store", prt ty1, prt op1 ++ ",", prt ty2, prt op2]

instance Printer Operand where
    prt (Loc ident) = prt ident 
    prt (Glob ident) = prt ident
    prt (ConstBool True) = "true"
    prt (ConstBool False) = "false"
    prt (ConstInt int) = show int
    prt Null = "null"

instance Printer Type where
    prt Void = "void"
    prt Size1 = "i1"
    prt Size8 = "i8"
    prt Size32 = "i32"
    prt (Ptr ty) = (prt ty) ++ "*"
    prt (Arr int ty) = concat ["[", show int, " x ", prt ty, "]"]
    prt (TypeClass ident) = prt ident

instance Printer LocalIdent where
    prt (LocalIdent str) = '%':str

instance Printer GlobalIdent where
    prt (GlobalIdent str) = '@':str

instance Printer Cond where
    prt EQU = "eq"
    prt NE = "ne"
    prt GTH = "sgt"
    prt GE = "sge"
    prt LTH = "slt"
    prt LE = "sle"

instance Printer Op where
    prt Plus = "add"
    prt Minus = "sub"
    prt Times = "mul"
    prt Div = "sdiv"
    prt Mod = "mod"