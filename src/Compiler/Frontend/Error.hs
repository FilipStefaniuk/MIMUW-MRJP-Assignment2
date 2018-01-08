-- module Frontend.Error where

-- import AbsLatte

-- TODO Group the errors
-- TODO Add Colors to errors
-- TODO Add Line numbers



    
--       InClassError Ident Error
--     | InFunctionError Ident Error
--     | MultipleFieldDeclarations Ident
--     | MultipleMethodDeclarations Ident
--     | MultipleFunctionDeclarations Ident
--     | NoMainFunction
--     | VoidArgument Ident 
--     | MultipleArgumentDeclarations Ident
--     | NoReturnStatement
--     | VoidReturn
--     | WrongTypes
--     | ErrorTest String
--     | VoidField Ident
--     | WrongMainFunctionType
--     | InheritenceError Ident
--     | ConditionTypeError
--     | IncDecTypeError
--     | WrongReturnType Type Type
--     | IncompatibleTypes Type Type
--     | MultipleVariableDeclarations Ident
--     | UndeclaredVariable Ident
--     | InheritenceCycle




--     show WrongTypes = "wrongTypes"
--     show (ErrorTest string) = "Testing: '" ++ string ++ "'"
--     show InheritenceCycle = "inheritence cycle detected"
--     show (UndeclaredVariable (Ident name)) = "variable '" ++ name ++ "' is undeclared"
--     show NoReturnStatement = "missing return statement"
--     show (InFunctionError (Ident name) err) = "in Function '" ++ name ++ "' " ++ show err 
--     show (InClassError (Ident name) err) = "in Class '" ++ name ++ "' " ++ show err
--     show (WrongReturnType ftype exprtype) = "return with type '" ++ show exprtype ++"' in function with return type '" ++ show ftype ++ "'"
--     show VoidReturn = "void return in non void function"
--     show (MultipleVariableDeclarations (Ident name)) = "multiple declarations of variable '" ++ name ++ "'"
--     show (MultipleFieldDeclarations (Ident name)) = "multiple declarations of field '" ++ name ++ "'"
--     show (MultipleMethodDeclarations (Ident name)) = "multiple declarations of method '" ++ name ++ "'"
--     show (MultipleArgumentDeclarations (Ident name)) = "multiple declarations of argument '" ++ name ++ "'"
--     show (MultipleFunctionDeclarations (Ident name)) = "multiple declarations of function '" ++ name ++ "'"
--     show (VoidArgument (Ident name)) = "argument '" ++ name ++ "' declared as void"
--     show (VoidField (Ident name)) = "field '" ++ name ++ "' declared as void"
--     show NoMainFunction = "no main function declared"
--     show WrongMainFunctionType = "main function should return int and have no arguments"
--     show (InheritenceError (Ident name)) = "ancestor class '" ++ name ++ "' not declared"
--     show ConditionTypeError = "condition expression must be type 'bool'"
--     show IncDecTypeError = "operand for unary operation ++ or -- must be type 'int'"
--     show (IncompatibleTypes type1 type2) = "incompatible types: '" ++ show type1 ++ "' and '" ++ show type2 ++ "'"
