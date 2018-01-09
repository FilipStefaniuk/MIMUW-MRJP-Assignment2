module Frontend.CompilerError where

import Data.Maybe

import AbsLatte
import Frontend.Types

import Control.Monad.Error

type Loc = Maybe (Int, Int)

data CompilerError = 
    CompilerError String
  | CompilerErrorLn Loc String
  | InFunctionError String CompilerError
  | InClassError String CompilerError

instance Error CompilerError where
    noMsg = CompilerError $ "internal error"
    strMsg str = CompilerError $ str

instance Show CompilerError where
    show (CompilerError string) = "error: " ++ string ++ "\n"
    show (CompilerErrorLn nr string) = let (ln, col) = fromJust nr in (show ln) ++ ":" ++ (show col) ++ ": error: " ++ string ++ "\n"
    show (InFunctionError name error) = (show error) ++ "\tin function: " ++ name ++ "\n"
    show (InClassError name error) = (show error) ++ "\tin class: " ++ name ++ "\n"

noMainError :: CompilerError
noMainError = CompilerError $ "no main function found\n\tprogram requires main function with sygnature 'int main()'"

multipleClassDeclarationError :: Loc -> Ident -> CompilerError
multipleClassDeclarationError nr (Ident name) = CompilerErrorLn nr $ "multiple declarations of class '" ++ name ++ "'"

multipleFunctionDeclarationError :: Loc -> Ident -> CompilerError
multipleFunctionDeclarationError nr (Ident name) = CompilerErrorLn nr $ "multiple declarations of function '" ++ name ++ "'"

multipleFieldDeclarationError :: Loc -> Ident -> CompilerError
multipleFieldDeclarationError nr (Ident name) = CompilerErrorLn nr $ "multiple declarations of field '" ++ name ++ "'"

multipleMethodDeclarationError :: Loc -> Ident -> CompilerError
multipleMethodDeclarationError nr (Ident name) = CompilerErrorLn nr $ "multiple declarations of method '" ++ name ++ "'"

multipleArgumentDeclarationError :: Loc -> Ident -> CompilerError
multipleArgumentDeclarationError nr (Ident name) = CompilerErrorLn nr $ "multiple declarations of argument '" ++ name ++ "'"

voidFieldError :: Loc -> CompilerError
voidFieldError nr = CompilerErrorLn nr $ "class field may not have '" ++ (show TVoid) ++ "' type"

voidArgumentError :: Loc -> CompilerError
voidArgumentError nr = CompilerErrorLn nr $ "argument may not have '" ++ (show TVoid) ++ "' type"

noReturnStmtError :: CompilerError
noReturnStmtError = CompilerError $ "missing return statement"

inheritenceCycleError :: CompilerError
inheritenceCycleError = CompilerError $ "inheritence cycle detected"

noAncestorClassError :: Ident -> CompilerError
noAncestorClassError (Ident name) = CompilerError $ "no ancestor class named '" ++ name ++ "' declared"

overrideError :: CompilerError
overrideError = CompilerError $ "override error\n\toverriden methods must have the same signature" 

wrongReturnTypeError :: Loc -> TType -> TType -> CompilerError
wrongReturnTypeError nr type1 type2 = CompilerErrorLn nr $ "wrong return type\n\texpected type: '" ++ (show type1) ++ "', actual type: '" ++ (show type2) ++ "'"

voidRetInNonVoidError :: CompilerError
voidRetInNonVoidError = CompilerError $ "void return in non void function"

wrongTypesError :: Loc -> TType -> TType -> CompilerError
wrongTypesError nr type1 type2 = CompilerErrorLn nr $ "incompatible types\n\texpected type: '" ++ (show type1) ++ "', actual type: '" ++ (show type2) ++ "'"

wrongCondExprType :: Loc -> CompilerError
wrongCondExprType nr = CompilerErrorLn nr $ "condition expression must have type '" ++ (show TBool) ++ "'"

incrDecrError :: Loc -> CompilerError
incrDecrError nr = CompilerErrorLn nr $ "operand for unary operation ++ or -- must be type '" ++ (show TInt) ++ "'"

iterationError :: Loc -> TType -> CompilerError
iterationError nr type_ = CompilerErrorLn nr $ "unable to iterate over non array type '" ++ (show type_) ++ "'" 

variableRedefinitionError :: Loc -> Ident -> CompilerError
variableRedefinitionError nr (Ident name) = CompilerErrorLn nr $ "redefinition of variable '" ++ name ++ "'"

undeclaredIdentifierError :: Loc -> Ident -> CompilerError
undeclaredIdentifierError nr (Ident name) = CompilerErrorLn nr $ "use of undeclared identifier '" ++ name ++"'"

arrayIndexTypeError :: Loc -> CompilerError
arrayIndexTypeError nr = CompilerErrorLn nr $ "array index must be type '" ++ (show TInt) ++ "'"

arrayTypeRequiredError :: Loc -> CompilerError
arrayTypeRequiredError nr = CompilerErrorLn nr $ "not an array type"

arrayLengthTypeError :: Loc -> CompilerError
arrayLengthTypeError nr = CompilerErrorLn nr $ "array length must be type '" ++ (show TInt) ++ "'"

unknownTypeName :: Loc -> Ident -> CompilerError
unknownTypeName nr (Ident name) = CompilerErrorLn nr $ "unknown type name '" ++ name ++ "'"

noFieldError :: Loc -> Ident -> Ident -> CompilerError
noFieldError nr (Ident className) (Ident fieldName) = CompilerErrorLn nr $ "no field named '" ++ fieldName ++ "' in class '" ++ className ++ "'" 

noMethodError :: Loc -> Ident -> Ident -> CompilerError
noMethodError nr (Ident className) (Ident methodName) = CompilerErrorLn nr $ "no methond named '" ++ methodName ++ "' in class '" ++ className ++ "'"

classTypeError :: Loc -> CompilerError
classTypeError nr = CompilerErrorLn nr $ "not a class type"

functionCallError :: Loc -> Ident -> CompilerError
functionCallError nr (Ident name) = CompilerErrorLn nr $ "wrong argument types in function call '" ++ name ++ "'"

methodCallError :: Loc -> Ident -> CompilerError
methodCallError nr (Ident name) = CompilerErrorLn nr $ "wrong argument types in method call '" ++ name ++ "'"

binaryOperatorTypesError :: Loc -> TType -> TType -> String -> CompilerError
binaryOperatorTypesError nr type1 type2 op = CompilerErrorLn nr $ "incompatible types '" ++ (show type1) ++ "' and '" ++ (show type2) ++ "' for binary operator '" ++ op ++ "'"

unaryOperatorTypeError :: Loc -> TType -> String -> CompilerError
unaryOperatorTypeError nr type_ op = CompilerErrorLn nr $ "incompatible type '" ++ (show type_) ++ "' for unary operator '" ++ op ++ "'"

unknownClassName :: Ident -> CompilerError
unknownClassName (Ident name) = CompilerError $ "unknown class name '" ++ name ++ "'"

thisOutsideClassError :: Loc -> CompilerError
thisOutsideClassError nr = CompilerErrorLn nr $ "usage of 'this' outside class"