module Frontend.Printer where

import Frontend.LLVM
import Data.Sequence as Seq
import Data.Foldable
import Data.List

printProgram :: Program -> String
printProgram (Program strings funcDefs) = unlines $ concat [
    printBuiltInFunctions,
    map printGlobalDef strings,
    [""],
    (intercalate [""]) . map printFunctionDef $ toList . Seq.reverse $ funcDefs]

printGlobal :: Global -> String
printGlobal (GlobalString int _ _) 
    | int == 0 = "@.str"
    | otherwise = "@.str." ++ (show int)

printGlobalType :: Global -> String
printGlobalType (GlobalString int type_ str) = printType type_

printGlobalDef :: Global -> String
printGlobalDef global@(GlobalString int type_ str) = concat [printGlobal global, " = constant ", printType type_, " c", str]

-- TODO change ' ' to unwords
printFunctionDef :: FunctionDef -> [String]
printFunctionDef (FunctionDef (FunctionAddr ident type_) args vars blocks) = concat [
        [concat ["define ", printType type_, " ", printIdent ident, "(", intercalate ", " (map (\reg -> printRegisterType reg ++ " " ++ printRegister reg) (toList . Seq.reverse $ args)),") {"]],
        map printAlloc $ toList . Seq.reverse $ vars,
        (intercalate [""] . map printBlock) $ toList blocks,
        ["}"]]

printIdent :: Ident -> String
printIdent (Ident str) = "@" ++ str 

printAlloc :: Alloc -> String
printAlloc (Alloc reg@(Register _ (Ptr ty))) = concat ["\t", printRegister reg, " = alloca ", printType ty]

printRegister :: Register -> String
printRegister (Register (UniqueId str) _) = '%':str

printRegisterType :: Register -> String
printRegisterType (Register _ ty) = printType ty

printType :: Type -> String
printType Size1 = "i1"
printType Size8 = "i8"
printType Size32 = "i32"
printType (Ptr ty) = (printType ty) ++ "*"
printType (Arr int ty) = concat ["[", show int, " x ", printType ty, "]"]

printBlock :: Block -> [String]
printBlock (Block label phi body end) = case label of
    Entry -> printBlockContent phi body end
    (Label (UniqueId l)) -> ("; <label>: " ++ l):(printBlockContent phi body end)
  where
    printBlockContent :: Seq.Seq Phi -> Seq.Seq Instruction -> BlockEnd -> [String]
    printBlockContent phi body end = concat [
        map printPhi $ toList phi,
        map printInstruction $ toList . Seq.reverse $ body,
        [printBlockEnd end]] 

printInstruction :: Instruction -> String
printInstruction (InstrBinOp reg binOp op1 op2) = concat [
    "\t", printRegister reg, " = ", printOperator binOp, " ", printOperandType op1, " ", printOperand op1, ", ", printOperand op2]
printInstruction (InstrVoidCall (FunctionAddr ident ty) operands) = concat ["\tcall void ", printIdent ident,"(", intercalate ", " $ map (\op -> (printOperandType op) ++ " " ++ (printOperand op)) operands, ")"]
printInstruction (InstrGetElementPtr reg operand) = concat [
    "\t", printRegister reg, " = getelementptr ", printOperandType operand, ", ", printOperandType operand, "*"]
printInstruction (InstrCall reg (FunctionAddr ident ty) operands) = concat ["\t", printRegister reg, " = call ", printType ty," ",printIdent ident,"(", intercalate ", " $ map (\op -> (printOperandType op) ++ " " ++ (printOperand op)) operands, ")"]
-- prt (InstrCall reg [Operand]
-- prt (InstrCmp reg Cond Operand Operand
printInstruction (InstrLoad reg1 reg2) = concat [
    "\t", printRegister reg1, " = load ", printRegisterType reg1, ", ", printRegisterType reg2, " ", printRegister reg2]
printInstruction (InstrStore op reg) = concat [
    "\tstore ", printOperandType op, " ", printOperand op, ", ", printRegisterType reg, " ", printRegister reg]

printOperator :: Op -> String
printOperator Plus = "add"
printOperator Minus = "sub"
printOperator Times = "mul"
printOperator Div = "sdiv"
printOperator Mod = "mod"

printPhi :: Phi -> String
printPhi (Phi reg branches) = concat["\t", printRegister reg, " = phi ", printRegisterType reg, " ", intercalate ", " (map printPhiBranch branches)]

printPhiBranch :: PhiBranch -> String
printPhiBranch (PhiBranch label op) = concat ["[ ", printOperand op, ", ", printLabel label, " ]"]

printLabel :: Label -> String
printLabel (Label (UniqueId str)) = "%" ++ str

printBlockEnd :: BlockEnd -> String
printBlockEnd (BlockEndBranch label) = "\tbr label " ++ (printLabel label)  
printBlockEnd (BlockEndReturn op) = concat ["\tret ", printOperandType op, " ", printOperand op]
printBlockEnd BlockEndReturnVoid = "\tret void"
printBlockEnd BlockEndNone = ""
printBlockEnd (BlockEndBranchCond op label1 label2) = concat [
    "\tbr ", printOperandType op, " ", printOperand op, ", label ", printLabel label1, ", label ", printLabel label2]

printOperand :: Operand -> String
printOperand (Reg reg) = printRegister reg
printOperand Null = "null"
printOperand (ConstBool True) = "true"
printOperand (ConstBool False) = "false"
printOperand (ConstInt int) = (show int)

printOperandType :: Operand -> String
printOperandType (Reg reg) = printRegisterType reg
printOperandType (Glob glob) = printGlobalType glob
printOperandType (ConstBool _) = printType Size1
printOperandType (ConstInt _) = printType Size32

printBuiltInFunctions :: [String]
printBuiltInFunctions = [
    "declare void @printInt(i32)",
    "declare void @error()",
    "declare i32 @readInt()",
    "declare i8* @readString()",
    "declare i8* @_appendString(i8*, i8*)",
    "declare i8* @malloc(i32)"]