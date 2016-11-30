-- SymbolTable.hs
-- compilation:
--      ghc --make SymbolTableStart.hs -o symTableStart
--
-- usage for testing the structure:
--      ./symTableStart

module SymbolTable where

import Data.Map as M

import Util

-- ========================= SymbolTable data structure

-- Scope is outermost program scope and the stack of strings keeps track
-- of our current scope nesting.
data SymbolTable = SymTab Scope [String]
    deriving (Show,Eq)

type Scope = M.Map String STE

data STE
    =   ClassSTE    Scope
    |   MethodSTE   (Scope, TypeSig)
    |   VarSTE      Type String Int Int -- Type, Name, Base, Offset
    deriving (Show,Eq)

-- ========================= Functions for SymbolTable
emptyScope :: Scope
emptyScope = M.fromList []

emptySymTab :: SymbolTable
emptySymTab = SymTab emptyScope []

-- String is the class name of class to insert.
-- Inserts a class with an empty scope.
insertClass :: SymbolTable -> String -> SymbolTable
insertClass (SymTab scope nesting) classname =
    SymTab (M.insert classname (ClassSTE emptyScope) scope) nesting

setProgScope :: SymbolTable -> SymbolTable
setProgScope (SymTab scope list) = 
    (SymTab scope [])

-- Assume the string given is for a named scope.
-- SymbolTable will now push that scope on the scope stack.
pushScope :: SymbolTable -> String -> SymbolTable
pushScope (SymTab progScope nesting) scopename =
    (SymTab progScope (scopename:nesting))

-- SymbolTable will pop the current scope on the scope stack. CREATED
popScope :: SymbolTable -> SymbolTable
popScope (SymTab progScope (curr:nesting)) =
    (SymTab progScope nesting)
popScope (SymTab progScope []) =
    (SymTab progScope [])

-- String is the method name of method to insert.
-- Inserts a method with an empty scope.
-- Assumes already in a class scope.
-- TODO: Will need to include method type signature. CREATED
insertMethod :: SymbolTable -> String -> TypeSig -> SymbolTable
insertMethod (SymTab progScope [classname]) methodname tsig@(TS params ret) =
    let
        (Just (ClassSTE classScope)) = M.lookup classname progScope
        doubleDef = checkDoubleDef classScope methodname
        classScope_new  = M.insert methodname (MethodSTE (emptyScope, tsig)) classScope
        st1 = SymTab  (M.insert classname (ClassSTE classScope_new) progScope) [classname] -- without parameters in method scope
        st2 = pushScope st1 methodname -- push methodscope
        st3 = insertParameters st2 2 params -- insert all parameters into method scope
    in
        if doubleDef then
            error ("Redefined symbol " ++ methodname ++ "\nErrors found while building symbol table" )
        else
            popScope st3 -- return symbol table back in class scope TODO: Do we want to return in class or method scope??

-- Given some class name and method name, lookup the type signature of the method
lookupTypeSig :: SymbolTable -> String -> String -> TypeSig
lookupTypeSig (SymTab progScope stack) classname methodname =
    let
        classScope = namedScopeLookup progScope classname
        (methodScope, tsig) = namedScopeLookup' classScope methodname
    in
        tsig

-- Assumes we are already in the correct scope of a method, grabs the return type of current method scope
getReturn :: SymbolTable -> Type
getReturn st@(SymTab scope [methodname,classname]) = lookupReturn st classname methodname

lookupReturn :: SymbolTable -> String -> String -> Type -- TODO: do we even use this function anywhere?
lookupReturn st classname methodname =
    let
        (TS params returnType) = lookupTypeSig st classname methodname
    in
        returnType

-- Called by insertMethod, calls insertParam, recursively adds all method parameters to method scope
insertParameters :: SymbolTable -> Int -> [(String,Type)] -> SymbolTable
insertParameters st offset [] = st
insertParameters st offset ((pname, ptype):ps) =
    let
        (st2, offset2) = insertParam st offset pname ptype
    in
        insertParameters st2 offset2 ps

-- Insert a parameter into the current scope.
-- TODO: will eventually want this function to compute offset
insertParam :: SymbolTable -> Int -> String -> Type -> (SymbolTable, Int)
insertParam (SymTab progScope [methodname,classname]) offset param pType =
    let
        -- unravel data structure to find all scopes
        classScope  = namedScopeLookup progScope classname
        (methodScope, tsig) = namedScopeLookup' classScope methodname

        -- create new versions of all the scopes
        offset2 = offset + typeToBytes pType
        methodScope_new = M.insert  param (VarSTE pType param (-1) offset2)
                                    methodScope
        classScope_new = M.insert   methodname (MethodSTE (methodScope_new, tsig))
                                    classScope
    in
        (SymTab  (M.insert classname (ClassSTE classScope_new) progScope)
                [methodname,classname],
        offset2)

-- helper function to insertParam, determines offset based on type
typeToBytes :: Type -> Int
typeToBytes t
    | t == IntType = 2
    | otherwise = 1

-- Given some current scope, and a parameter name, lookup the type of the parameter

-- options -> lookupTypeSig, lookupParamType, lookupParamOffset
lookupParamType :: SymbolTable -> String -> Type
lookupParamType (SymTab progScope (methodname:classname:rest)) paramname =
    let
        classScope  = namedScopeLookup progScope classname
        (methodScope, tsig) = namedScopeLookup' classScope methodname
    in
        case M.lookup paramname methodScope of
            Nothing -> error (paramname++" not found in "++methodname++ " or "++classname)
            (Just (VarSTE vartype name base offset)) -> vartype
            (Just x) -> error ("Parameter without a type, "++(show x))

lookupParamOffset :: SymbolTable -> String -> Int
lookupParamOffset (SymTab progScope [methodname,classname]) paramname =
    let
        classScope  = namedScopeLookup progScope classname
        (methodScope, tsig) = namedScopeLookup' classScope methodname
    in
        case M.lookup paramname methodScope of
            Nothing -> error (paramname++" not found in "++methodname++ " or "++classname)
            (Just (VarSTE vartype name base offset)) -> offset
            (Just x) -> error ("Parameter without a type, "++(show x))

-- Given some scope and a string to lookup, find the embedded scope or throw
-- an error.
-- CANNOT be used for methods
namedScopeLookup :: Scope -> String -> Scope
namedScopeLookup outer name =
    case M.lookup name outer of
        -- Nothing -> error ("Undeclared class of name [" ++ name ++ "] in new operator")
        Nothing -> error ("\n------\nSymbol Table Error:\nOuter scope: "++show outer++"\n------\nDoes not have class of: "++ name++"\n------")
        (Just (ClassSTE inner))  -> inner
        (Just x) -> error ("STE without a scope, "++ (show x))

-- Only used for methods
namedScopeLookup' :: Scope -> String -> (Scope, TypeSig)
namedScopeLookup' outer name =
    case M.lookup name outer of
        -- Nothing -> error ("Method " ++ name ++ " does not exist in class " ++ outer)
        Nothing -> error ("\n------\nSymbol Table Error:\nOuter scope: "++show outer++"\n------\nDoes not have method of: "++ name++"\n------")
        (Just (MethodSTE (inner))) -> inner
        (Just x) -> error ("STE without a scope, "++ (show x))

checkDoubleDef :: Scope -> String -> Bool
checkDoubleDef outer name =
    case M.lookup name outer of
        -- Nothing -> error ("Method " ++ name ++ " does not exist in class " ++ outer)
        Nothing -> False
        (Just (MethodSTE (inner))) -> True
        (Just x) -> error ("When checking for double definition, found STE without a scope, "++ (show x))

-- ========================= Testing
-- Testing
checkExpect:: (Eq a) => (Show a) => a -> a -> String
checkExpect check expect =
    if check /= expect
    then "checkExpect FAILED, got: "
         ++ (show check) ++ ", expected: " ++ (show expect)
    else "checkExpect PASSED"

