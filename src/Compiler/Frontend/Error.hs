module Frontend.Error where

import Data.Maybe

import AbsLatte
import Frontend.Types

type Loc = Maybe (Int, Int)

data Error = 
    Error String
  | ErrorLn Loc String
  | InFunctionError String Error
  | InClassError String Error

instance Show Error where
    show (Error string) = "error: " ++ string ++ "\n"
    show (ErrorLn nr string) = let (ln, col) = fromJust nr in (show ln) ++ ":" ++ (show col) ++ ": error: " ++ string ++ "\n"
    show (InFunctionError name error) = (show error) ++ "\tin function: " ++ name ++ "\n"
    show (InClassError name error) = (show error) ++ "\tin class: " ++ name ++ "\n"

noMainError :: Error
noMainError = Error $ "no main function found\n\tprogram requires main function with sygnature 'int main()'"

multipleClassDeclarationError :: Loc -> Ident -> Error
multipleClassDeclarationError nr (Ident name) = ErrorLn nr $ "multiple declarations of class '" ++ name ++ "'"

multipleFunctionDeclarationError :: Loc -> Ident -> Error
multipleFunctionDeclarationError nr (Ident name) = ErrorLn nr $ "multiple declarations of function '" ++ name ++ "'"

multipleFieldDeclarationError :: Loc -> Ident -> Error
multipleFieldDeclarationError nr (Ident name) = ErrorLn nr $ "multiple declarations of field '" ++ name ++ "'"

multipleMethodDeclarationError :: Loc -> Ident -> Error
multipleMethodDeclarationError nr (Ident name) = ErrorLn nr $ "multiple declarations of method '" ++ name ++ "'"

multipleArgumentDeclarationError :: Loc -> Ident -> Error
multipleArgumentDeclarationError nr (Ident name) = ErrorLn nr $ "multiple declarations of argument '" ++ name ++ "'"

voidFieldError :: Loc -> Error
voidFieldError nr = ErrorLn nr $ "class field may not have '" ++ (show TVoid) ++ "' type"

voidArgumentError :: Loc -> Error
voidArgumentError nr = ErrorLn nr $ "argument may not have '" ++ (show TVoid) ++ "' type"

noReturnStmtError :: Error
noReturnStmtError = Error $ "missing return statement"

inheritenceCycleError :: Error
inheritenceCycleError = Error $ "inheritence cycle detected"

noAncestorClassError :: Ident -> Error
noAncestorClassError (Ident name) = Error $ "no ancestor class named '" ++ name ++ "' declared"

overrideError :: Error
overrideError = Error $ "override error\n\toverriden methods must have the same signature" 

wrongReturnTypeError :: Loc -> TType -> TType -> Error
wrongReturnTypeError nr type1 type2 = ErrorLn nr $ "wrong return type\n\texpected type: '" ++ (show type1) ++ "', actual type: '" ++ (show type2) ++ "'"

voidRetInNonVoidError :: Error
voidRetInNonVoidError = Error $ "void return in non void function"

wrongTypesError :: Loc -> TType -> TType -> Error
wrongTypesError nr type1 type2 = ErrorLn nr $ "incompatible types\n\texpected type: '" ++ (show type1) ++ "', actual type: '" ++ (show type2) ++ "'"

wrongCondExprType :: Loc -> Error
wrongCondExprType nr = ErrorLn nr $ "condition expression must have type '" ++ (show TBool) ++ "'"

incrDecrError :: Loc -> Error
incrDecrError nr = ErrorLn nr $ "operand for unary operation ++ or -- must be type '" ++ (show TInt) ++ "'"

iterationError :: Loc -> TType -> Error
iterationError nr type_ = ErrorLn nr $ "unable to iterate over non array type '" ++ (show type_) ++ "'" 

variableRedefinitionError :: Loc -> Ident -> Error
variableRedefinitionError nr (Ident name) = ErrorLn nr $ "redefinition of variable '" ++ name ++ "'"

undeclaredIdentifierError :: Loc -> Ident -> Error
undeclaredIdentifierError nr (Ident name) = ErrorLn nr $ "use of undeclared identifier '" ++ name ++"'"

arrayIndexTypeError :: Loc -> Error
arrayIndexTypeError nr =  ErrorLn nr $ "array index must be type '" ++ (show TInt) ++ "'"

arrayTypeRequiredError :: Loc -> Error
arrayTypeRequiredError nr = ErrorLn nr $ "not an array type"

arrayLengthTypeError :: Loc -> Error
arrayLengthTypeError nr = ErrorLn nr $ "array length must be type '" ++ (show TInt) ++ "'"

unknownTypeName :: Loc -> Ident -> Error
unknownTypeName nr (Ident name) = ErrorLn nr $ "unknown type name '" ++ name ++ "'"

noFieldError :: Loc -> Ident -> Ident -> Error
noFieldError nr (Ident className) (Ident fieldName) = ErrorLn nr $ "no field named '" ++ fieldName ++ "' in class '" ++ className ++ "'" 

noMethodError :: Loc -> Ident -> Ident -> Error
noMethodError nr (Ident className) (Ident methodName) = ErrorLn nr $ "no methond named '" ++ methodName ++ "' in class '" ++ className ++ "'"

classTypeError :: Loc -> Error
classTypeError nr = ErrorLn nr $ "not a class type"

functionCallError :: Loc -> Ident -> Error
functionCallError nr (Ident name) = ErrorLn nr $ "wrong argument types in function call '" ++ name ++ "'"

methodCallError :: Loc -> Ident -> Error
methodCallError nr (Ident name) = ErrorLn nr $ "wrong argument types in method call '" ++ name ++ "'"

binaryOperatorTypesError :: Loc -> TType -> TType -> String -> Error
binaryOperatorTypesError nr type1 type2 op = ErrorLn nr $ "incompatible types '" ++ (show type1) ++ "' and '" ++ (show type2) ++ "' for binary operator '" ++ op ++ "'"

unaryOperatorTypeError :: Loc -> TType -> String -> Error
unaryOperatorTypeError nr type_ op = ErrorLn nr $ "incompatible type '" ++ (show type_) ++ "' for unary operator '" ++ op ++ "'"

unknownClassName :: Ident -> Error
unknownClassName (Ident name) = Error $ "unknown class name '" ++ name ++ "'"

thisOutsideClassError :: Loc -> Error
thisOutsideClassError nr = ErrorLn nr $ "usage of 'this' outside class"