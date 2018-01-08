module Frontend.Types where

import AbsLatte

data TType 
    = TVoid | TInt | TStr | TBool | TNull | TArr TType | TObj Ident
    deriving (Eq, Read)

instance Show TType where
    show TVoid = "void"
    show TInt = "int"
    show TStr = "string"
    show TBool = "boolean"
    show TNull = "null"
    show (TArr type_) = (show type_) ++ "[]"
    show (TObj (Ident str)) = str

getArrElementType :: TType -> Maybe TType
getArrElementType (TArr type_) = Just type_
getArrElementType _ = Nothing

ttype :: Type a -> TType
ttype (Void _) = TVoid
ttype (Int _) = TInt
ttype (Str _) = TStr
ttype (Bool _) = TBool
ttype (Obj _ ident) = TObj ident
ttype (Arr _ type_)  = TArr $ ttype type_

