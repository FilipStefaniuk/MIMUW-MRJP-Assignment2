module LLVM where

import Data.Map as Map
import Data.Sequence as Seq
import Data.List as List

newtype GlobalIdent = GlobalIdent String
    deriving (Eq, Ord, Show)

newtype LocalIdent = LocalIdent String
    deriving (Eq, Ord, Show)

data Operand 
    = Loc LocalIdent 
    | Glob GlobalIdent
    | ConstBool Bool
    | ConstInt Integer
    | Null
    deriving (Eq, Ord, Show)

data Type 
    = Size1 
    | Size8 
    | Size32 
    | Ptr Type 
    | Arr Integer Type 
    | Void 
    | TypeClass LocalIdent
    deriving (Eq, Ord, Show)

data Phi = Phi LocalIdent Type [PhiBranch] 
    deriving (Eq, Ord, Show)

data PhiBranch = PhiBranch LocalIdent Operand 
    deriving (Eq, Ord, Show)
    
data CodeBlock = CodeBlock LocalIdent [Phi] [Instruction] BlockEnd
    deriving (Eq, Ord, Show)

data BlockEnd
    = BlockEndBranch LocalIdent
    | BlockEndBranchCond Type Operand LocalIdent LocalIdent
    | BlockEndReturn Type Operand
    | BlockEndReturnVoid
    | BlockEndNone
    deriving (Eq, Ord, Show)

data Alloc = Alloc LocalIdent Type
    deriving (Eq, Ord, Show)

data Instruction
    = InstrBinOp LocalIdent Op Type Operand Operand
    | InstrCall LocalIdent Type GlobalIdent [(Type, Operand)]
    | InstrVoidCall GlobalIdent [(Type, Operand)]
    | InstrGetElementPtr LocalIdent Type Type Operand [Operand]
    | InstrCmp LocalIdent Cond Type Operand Operand
    | InstrBitcast LocalIdent Type Operand Type
    | InstrTrunc LocalIdent Type Operand Type
    | InstrZext LocalIdent Type Operand Type
    | InstrLoad LocalIdent Type Type LocalIdent
    | InstrStore Type Operand Type LocalIdent
    deriving (Eq, Ord, Show)

data FunctionDef = FunctionDef Type GlobalIdent [(Type, LocalIdent)] [Alloc] [CodeBlock]
    deriving (Eq, Ord, Show)

data ClassDef = ClassDef LocalIdent [Type]
    deriving (Eq, Ord, Show)

data StringDef = StringDef GlobalIdent Type String
    deriving (Eq, Ord, Show)

data Program = Program [ClassDef] [StringDef] [FunctionDef] 
    deriving (Eq, Ord, Show)

data Op
    = Plus
    | Minus
    | Times
    | Div
    | Mod
    deriving (Eq, Ord, Show)

data Cond
    = LTH
    | LE
    | GTH
    | GE
    | EQU
    | NE
    deriving (Eq, Ord, Show)