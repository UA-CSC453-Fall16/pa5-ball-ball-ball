{-
██████╗ ██╗   ██╗██╗██╗     ██████╗ ███████╗████████╗██╗  ██╗███████╗
██╔══██╗██║   ██║██║██║     ██╔══██╗██╔════╝╚══██╔══╝██║  ██║██╔════╝
██████╔╝██║   ██║██║██║     ██║  ██║███████╗   ██║   ███████║███████╗
██╔══██╗██║   ██║██║██║     ██║  ██║╚════██║   ██║   ██╔══██║╚════██║
██████╔╝╚██████╔╝██║███████╗██████╔╝███████║   ██║██╗██║  ██║███████║
╚═════╝  ╚═════╝ ╚═╝╚══════╝╚═════╝ ╚══════╝   ╚═╝╚═╝╚═╝  ╚═╝╚══════╝
-}
-- BuildST.hs
--
-- Step (4) of compiling.
--
-- BuildST module takes the AST generated by Parser.hs and generates a symbold table using
-- the functions in SymbolTable.hs
--

module BuildST where

import SymbolTable  -- For SymbolTable type and functions
import Util         -- For AST type

-- entry point
genSymbolTable :: AST -> SymbolTable
genSymbolTable root = travisty (root, emptySymTab)

-- traverse is a Haskell keyword and cannot be used as our function name, what a travisty
travisty :: (AST, SymbolTable) -> SymbolTable
travisty (Prog main_class [], st) =  st
travisty (Prog main_class other_class, st) =  traverseClass (other_class, st)

traverseClass :: ([AST], SymbolTable) -> SymbolTable
traverseClass ([], st) = st
traverseClass ((Class variables methods class_name):otherClasses, st) = 
    let
        st0 = insertClass st class_name
        st1 = pushScope st0 class_name
        st2 = traverseAllVariables (variables, st1)
        st3 = traverseAllMethods (methods, st2)
        st4 = popScope st3
    in
        traverseClass (otherClasses, st4)

traverseAllMethods :: (AST, SymbolTable) -> SymbolTable
traverseAllMethods ((MethDecl methods), st) = traverseAllMethods' (methods, st)

traverseAllMethods' :: ([AST], SymbolTable) -> SymbolTable
traverseAllMethods' ([], st)            = st
traverseAllMethods' ((method:rest), st) = 
    let
        st1 = traverseMethod (method, st)
    in 
        traverseAllMethods' (rest, st1)

traverseMethod :: (AST, SymbolTable) -> SymbolTable
traverseMethod ((Method localvars _ method_name typesig), st) =
    let
        st1 = insertMethod st method_name typesig
        st2 = pushScope st1 method_name
        st3 = traverseAllVariables (localvars, st2)
    in
        popScope st3

traverseAllVariables :: (AST, SymbolTable) -> SymbolTable
traverseAllVariables ((VarDecl variables), st) = traverseAllVariables' (variables, st)

traverseAllVariables' :: ([AST], SymbolTable) -> SymbolTable
traverseAllVariables' ([], st) = st
traverseAllVariables' ((variable:rest), st) = 
    let
        st1 = traverseVariable (variable, st)
    in 
        traverseAllVariables' (rest, st1)

traverseVariable :: (AST, SymbolTable) -> SymbolTable
traverseVariable ((Variable vtype vname), st) = insertVariable st vname vtype