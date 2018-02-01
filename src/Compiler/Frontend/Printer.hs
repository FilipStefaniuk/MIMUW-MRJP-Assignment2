module Frontend.Printer where

import Frontend.LLVM
import Data.Sequence as Seq
import Data.Foldable
import Data.List

printProgram :: Program -> String
printProgram (Program funcDefs) = unlines . (intercalate [""]) . map printFunctionDef $ toList . Seq.reverse $ funcDefs


-- TODO change ' ' to unwords
printFunctionDef :: FunctionDef -> [String]
printFunctionDef (FunctionDef type_ (Ident str) args vars blocks) = concat [
        [concat ["define ", printType type_, " @", str, "(", intercalate ", " (map (\reg -> printRegisterType reg ++ " " ++ printRegister reg) (toList . Seq.reverse $ args)),") {"]],
        map printAlloc $ toList . Seq.reverse $ vars,
        (intercalate [""] . map printBlock) $ toList blocks,
        ["}"]]

printAlloc :: Alloc -> String
printAlloc (Alloc reg) = concat ["\t", printRegister reg, " = alloca ", printRegisterType reg]

printRegister :: Register -> String
printRegister (Register (UniqueId int) _) = '%':(show int)

printRegisterType :: Register -> String
printRegisterType (Register _ ty) = printType ty

printType :: Type -> String
printType Size1 = "i1"
printType Size8 = "i8"
printType Size32 = "i32"
printType (Ptr ty) = (printType ty) ++ "*"

printBlock :: Block -> [String]
printBlock (Block (Label l) phi body end) 
        | l == 0 = printBlockContent phi body end
        | otherwise = ("; <label>: " ++ show l):(printBlockContent phi body end)
      where
        printBlockContent :: Seq.Seq Phi -> Seq.Seq Instruction -> BlockEnd -> [String]
        printBlockContent phi body end = concat [
            map printPhi $ toList phi,
            map printInstruction $ toList . Seq.reverse $ body,
            [printBlockEnd end]] 

printInstruction :: Instruction -> String
printInstruction (InstrBinOp reg binOp op1 op2) = concat [
    "\t", printRegister reg, " = ", printOperator binOp, " ", printOperandType op1, " ", printOperand op1, ", ", printOperand op2]
printInstruction (InstrVoidCall operands) = concat ["\tvoid @f(", intercalate ", " $ map printOperand operands, ")"]
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
printOperator Div = "div"
printOperator Mod = "mod"

printPhi :: Phi -> String
printPhi (Phi reg branches) = concat["\t", printRegister reg, " = phi ", printRegisterType reg, " ", intercalate ", " (map printPhiBranch branches)]

printPhiBranch :: PhiBranch -> String
printPhiBranch (PhiBranch label op) = concat ["[ ", printOperand op, ", ", printLabel label, " ]"]

printLabel :: Label -> String
printLabel (Label int) = '%':(show int)

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
printOperandType (ConstBool _) = printType Size1
printOperandType (ConstInt _) = printType Size32