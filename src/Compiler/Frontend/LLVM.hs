module Frontend.LLVM where

import Data.Map as Map
import Data.Sequence as Seq
import Data.List as List

newtype UniqueId = UniqueId Integer
    deriving (Eq, Ord)

newtype Label = Label Integer
    deriving (Eq, Ord)

newtype Ident = Ident String
    deriving (Eq, Ord)

data Register = Register UniqueId Type 
    deriving (Eq, Ord)

data Operand 
    = Reg Register 
    | Global Ident
    | ConstBool Bool
    | ConstInt Integer
    | Null
    deriving (Eq, Ord)

data Type = Size1 | Size8 | Size32 | Ptr Type 
    deriving (Eq, Ord)

data Phi = Phi Register [PhiBranch] 
    deriving Eq

data PhiBranch = PhiBranch Label Operand 
    deriving Eq
    
data Block = Block {
    _blockLabel :: Label,
    _blockPhi :: Seq.Seq Phi,
    _blockBody :: Seq.Seq Instruction,
    _blockEnd :: BlockEnd
} deriving Eq

data BlockEnd
    = BlockEndBranch Label
    | BlockEndBranchCond Operand Label Label
    | BlockEndReturn Operand
    | BlockEndReturnVoid
    | BlockEndNone
    deriving Eq

data Alloc = Alloc Register
    deriving Eq

data Instruction
    = InstrBinOp Register Op Operand Operand
    | InstrCall Register [Operand]
    | InstrVoidCall [Operand]
    | InstrCmp Register Cond Operand Operand
    | InstrLoad Register Register
    | InstrStore Operand Register
    deriving Eq

data FunctionDef = FunctionDef {
    _functionType :: Type,
    _functionName :: Ident,
    _functionArgs :: Seq.Seq Register,
    _localVars :: Seq.Seq Alloc,
    _blocks :: Map.Map Label Block
}

data Program = Program {
    _functionDefs :: Seq.Seq FunctionDef
}

data Op
    = Plus
    | Minus
    | Times
    | Div
    | Mod
    deriving Eq

data Cond
    = LTH
    | LE
    | GTH
    | GE
    | EQU
    | NE
    deriving Eq