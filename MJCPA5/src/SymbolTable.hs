{-
                                    < View in "Raw" on GitHub >
███████╗██╗   ██╗███╗   ███╗██████╗  ██████╗ ██╗  ████████╗ █████╗ ██████╗ ██╗     ███████╗   ██╗  ██╗███████╗
██╔════╝╚██╗ ██╔╝████╗ ████║██╔══██╗██╔═══██╗██║  ╚══██╔══╝██╔══██╗██╔══██╗██║     ██╔════╝   ██║  ██║██╔════╝
███████╗ ╚████╔╝ ██╔████╔██║██████╔╝██║   ██║██║     ██║   ███████║██████╔╝██║     █████╗     ███████║███████╗
╚════██║  ╚██╔╝  ██║╚██╔╝██║██╔══██╗██║   ██║██║     ██║   ██╔══██║██╔══██╗██║     ██╔══╝     ██╔══██║╚════██║
███████║   ██║   ██║ ╚═╝ ██║██████╔╝╚██████╔╝███████╗██║   ██║  ██║██████╔╝███████╗███████╗██╗██║  ██║███████║
╚══════╝   ╚═╝   ╚═╝     ╚═╝╚═════╝  ╚═════╝ ╚══════╝╚═╝   ╚═╝  ╚═╝╚═════╝ ╚══════╝╚══════╝╚═╝╚═╝  ╚═╝╚══════╝
                                    < View in "Raw" on GitHub >
-}
--
-- Not a step in compiling.
--
-- The functions symTabToString, genSTEs, and indent were adapted from PA4 PeerReview group 9748 to pretty print the SymbolTable

module SymbolTable where

import Data.Map as M

import Util

-- ========================= Functions for SymbolTable
emptyScope :: Scope
emptyScope = M.fromList []

emptySymTab :: SymbolTable
emptySymTab = SymTab emptyScope []

-- String is the class name of class to insert.
-- Inserts a class with an empty scope.
insertClass :: SymbolTable -> String -> SymbolTable
insertClass (SymTab scope nesting) classname =
    SymTab (M.insert classname (ClassSTE classname emptyScope 0) scope) nesting

setProgScope :: SymbolTable -> SymbolTable
setProgScope (SymTab scope list) = 
    (SymTab scope [])

-- Assume the string given is for a named scope.
-- SymbolTable will now push that scope on the scope stack.
pushScope :: SymbolTable -> String -> SymbolTable
pushScope (SymTab progScope nesting) scopename =
    (SymTab progScope (scopename:nesting))

-- SymbolTable will pop the current scope on the scope stack.
popScope :: SymbolTable -> SymbolTable
popScope (SymTab progScope (curr:nesting)) =
    (SymTab progScope nesting)
popScope (SymTab progScope []) =
    (SymTab progScope [])

-- String is the method name of method to insert.
-- Inserts a method with an empty scope.
-- Assumes already in a class scope.
-- Includes method type signature.
insertMethod :: SymbolTable -> String -> TypeSig -> SymbolTable
insertMethod (SymTab progScope [classname]) methodname tsig@(TS params ret) =
    let
        (Just (ClassSTE cname classScope coffset)) = M.lookup classname progScope
        doubleDef = checkDoubleDef classScope methodname
        classScope_new  = M.insert methodname (MethodSTE methodname (emptyScope, tsig) 3) classScope
        st1 = SymTab  (M.insert classname (ClassSTE cname classScope_new coffset) progScope) [classname] -- without parameters in method scope
        st2 = pushScope st1 methodname -- push methodscope
        st3 = insertParameters st2 params -- insert all parameters into method scope
    in
        if doubleDef then
            error ("Redefined symbol " ++ methodname ++ "\nErrors found while building symbol table" )
        else
            popScope st3 -- return symbol table back in class scope

-- Given some class name and method name, lookup the type signature of the method
lookupTypeSig :: SymbolTable -> String -> String -> TypeSig
lookupTypeSig (SymTab progScope [mmm,ccc]) "this" methodname =
    let
        (cname, classScope, coffset) = namedScopeLookup progScope ccc
        (mname, methodScope, tsig, moffset) = namedScopeLookup' classScope methodname
    in
        tsig
lookupTypeSig (SymTab progScope nesting) classname methodname =
    let
        (cname, classScope, coffset) = namedScopeLookup progScope classname
        (mname, methodScope, tsig, moffset) = namedScopeLookup' classScope methodname
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
insertParameters :: SymbolTable -> [(String,Type)] -> SymbolTable
insertParameters st [] = st
insertParameters st ((pname, ptype):ps) =
    let
        st2 = insertParam st pname ptype
    in
        insertParameters st2 ps

-- Insert a parameter into the current scope.
insertParam :: SymbolTable -> String -> Type -> SymbolTable
insertParam (SymTab progScope [methodname,classname]) param pType =
    let
        -- unravel data structure to find all scopes
        (cname, classScope, coffset)  = namedScopeLookup progScope classname
        (mname, methodScope, tsig, moffset) = namedScopeLookup' classScope methodname

        -- create new versions of all the scopes
        moffset2 = moffset + typeToBytes pType
        methodScope_new = M.insert  param (VarSTE pType param " Y " moffset)
                                    methodScope
        classScope_new = M.insert   methodname (MethodSTE mname (methodScope_new, tsig) moffset2)
                                    classScope
    in
        (SymTab  (M.insert classname (ClassSTE cname classScope_new coffset) progScope)
                [methodname,classname])

-- helper function to insertParam, determines offset based on type
typeToBytes :: Type -> Int
typeToBytes (ClassType _)   = 2
typeToBytes IntType         = 2
typeToBytes IntArrayType    = 2
typeToBytes ColorArrayType  = 2
typeToBytes MeggyToneType   = 2
typeToBytes VoidType        = 0
typeToBytes other           = 1

-- Given some current scope, and a parameter name, lookup the type of the parameter
lookupParamType :: SymbolTable -> String -> Type
lookupParamType st@(SymTab progScope (methodname:classname:rest)) paramname =
    let
        (cname, classScope, coffset) = namedScopeLookup progScope classname
        (mname, methodScope, tsig, moffset) = namedScopeLookup' classScope methodname
    in
        case M.lookup paramname methodScope of
            Nothing -> error (paramname++" not found in "++methodname++ " or "++classname)
            (Just (VarSTE vartype name base offset)) -> vartype
            (Just x) -> error ("Parameter without a type, "++(symTabToString st 0))

lookupParamOffset :: SymbolTable -> String -> Int
lookupParamOffset st@(SymTab progScope [methodname,classname]) paramname =
    let
        (cname, classScope, coffset) = namedScopeLookup progScope classname
        (mname, methodScope, tsig, moffset) = namedScopeLookup' classScope methodname
    in
        case M.lookup paramname methodScope of
            Nothing -> error (paramname++" not found in "++methodname++ " or "++classname)
            (Just (VarSTE vartype name base offset)) -> offset
            (Just x) -> error ("Parameter without a type, "++(symTabToString st 0))

-- Insert a variable declaration into the current scope.
insertVariable :: SymbolTable -> String -> Type -> SymbolTable
-- into a method
insertVariable (SymTab progScope [methodname,classname]) vname vtype =
    let
        -- unravel data structure to find all scopes
        (cname, classScope, coffset)  = namedScopeLookup progScope classname
        (mname, methodScope, tsig, moffset) = namedScopeLookup' classScope methodname

        -- create new versions of all the scopes
        moffset2 = moffset + typeToBytes vtype
        methodScope_new = M.insert  vname (VarSTE vtype vname " Y " moffset)
                                    methodScope
        classScope_new = M.insert   methodname (MethodSTE mname (methodScope_new, tsig) moffset2)
                                    classScope
    in
        (SymTab  (M.insert classname (ClassSTE cname classScope_new coffset) progScope)
                [methodname,classname])

-- into a class
insertVariable (SymTab progScope [classname]) vname vtype =
    let
        -- unravel data structure to find all scopes
        (cname, classScope, coffset)  = namedScopeLookup progScope classname

        -- create new versions of all the scopes
        coffset2 = coffset + typeToBytes vtype
        classScope_new = M.insert  vname (VarSTE vtype vname " Z " coffset)
                                    classScope
    in
        (SymTab  (M.insert classname (ClassSTE cname classScope_new coffset2) progScope)
                [classname])

-- Given some current scope, and a variable name, lookup the type of the variable
lookupVariableType :: SymbolTable -> String -> Type
lookupVariableType st@(SymTab progScope [methodname,classname]) vname =
    let
        (cname, classScope, coffset) = namedScopeLookup progScope classname
        (mname, methodScope, tsig, moffset) = namedScopeLookup' classScope methodname
    in
        case M.lookup vname methodScope of
            Nothing -> lookupVariableType (SymTab progScope [classname]) vname
            (Just (VarSTE vartype name base offset)) -> vartype
            (Just x) -> error ("Variable without a type, "++(symTabToString st 0))

lookupVariableType st@(SymTab progScope [classname]) vname =
    let
        (cname, classScope, coffset) = namedScopeLookup progScope classname
    in
        case M.lookup vname classScope of
            Nothing -> error (vname++" not found in "++classname)
            (Just (VarSTE vartype name base offset)) -> vartype
            (Just x) -> error ("Variable without a type, "++(symTabToString st 0))

lookupVariableType st vname = error "bad lookup"

-- Given some current scope, and a variable name, lookup the lower offset of the variable
lookupVariableOffset :: SymbolTable -> String -> Int
lookupVariableOffset st@(SymTab progScope [methodname,classname]) vname =
    let
        (cname, classScope, coffset) = namedScopeLookup progScope classname
        (mname, methodScope, tsig, moffset) = namedScopeLookup' classScope methodname
    in
        case M.lookup vname methodScope of
            Nothing -> lookupVariableOffset (SymTab progScope [classname]) vname
            (Just (VarSTE vartype name base offset)) -> offset
            (Just x) -> error ("Variable without a type, "++(symTabToString st 0))

lookupVariableOffset st@(SymTab progScope [classname]) vname =
    let
        (cname, classScope, coffset) = namedScopeLookup progScope classname
    in
        case M.lookup vname classScope of
            Nothing -> error (vname++" not found in "++classname)
            (Just (VarSTE vartype name base offset)) -> offset
            (Just x) -> error ("Variable without a type, "++(symTabToString st 0))

-- Given some current scope, and a variable name, lookup the base of the variable
lookupVariableBase :: SymbolTable -> String -> String
lookupVariableBase (SymTab progScope [methodname,classname]) vname =
    let
        (cname, classScope, coffset) = namedScopeLookup progScope classname
        (mname, methodScope, tsig, moffset) = namedScopeLookup' classScope methodname
    in
        case M.lookup vname methodScope of
            Nothing -> lookupVariableBase (SymTab progScope [classname]) vname
            (Just (VarSTE vartype name base offset)) -> base
            (Just x) -> error ("Variable without a type, "++(show x))

lookupVariableBase (SymTab progScope [classname]) vname =
    let
        (cname, classScope, coffset) = namedScopeLookup progScope classname
    in
        case M.lookup vname classScope of
            Nothing -> error (vname++" not found in "++classname)
            (Just (VarSTE vartype name base offset)) -> base
            (Just x) -> error ("Variable without a type, "++(show x))


-- Given some scope and a string to lookup, find the embedded scope or throw
-- an error.
-- NOTE: Assumes the caller passes the Program scope
-- CANNOT be used for methods
namedScopeLookup :: Scope -> String -> (String, Scope, Int)
namedScopeLookup outer name =
    case M.lookup name outer of
        -- Nothing -> error ("Undeclared class of name [" ++ name ++ "] in new operator")
        Nothing -> error ("\n------\nSymbol Table Error:\nOuter scope: "++show outer++"\n------\nDoes not have class of: "++ name++"\n------")
        (Just (ClassSTE cname inner offset))  -> (cname, inner, offset)
        (Just x) -> error ("STE without a scope, "++ (show x))

-- Only used for methods
-- NOTE: Assumes the caller passes the Class scope
namedScopeLookup' :: Scope -> String -> (String, Scope, TypeSig, Int)
namedScopeLookup' outer name =
    case M.lookup name outer of
        -- Nothing -> error ("Method " ++ name ++ " does not exist in class " ++ outer)
        Nothing -> error ("\n------\nSymbol Table Error:\nOuter scope: "++show outer++"\n------\nDoes not have method of: "++ name++"\n------")
        (Just (MethodSTE mname (scope, tsig) moffset)) -> (mname, scope, tsig, moffset)
        (Just x) -> error ("STE without a scope, "++ (show x))

checkDoubleDef :: Scope -> String -> Bool
checkDoubleDef outer name =
    case M.lookup name outer of
        -- Nothing -> error ("Method " ++ name ++ " does not exist in class " ++ outer)
        Nothing -> False
        (Just (MethodSTE mname (inner) moffset)) -> True
        (Just x) -> error ("When checking for double definition, found STE without a scope, "++ (show x))

-- Pretty print method to better visualize the symbol
-- table data structure.
symTabToString :: SymbolTable -> Int -> String
symTabToString (SymTab scope location) n =
    let
        stars = "********************************************\n"
        prefix = indent n
        intro = "Printing SymbolTable " ++ (show location) ++ ":\n"
        entries = "keys: " ++  (show (M.keys scope)) ++ "\n"
        stes = getSTEs (M.elems scope) (n+1)
    in
        prefix ++ stars ++ intro ++ prefix ++ entries ++ prefix ++ stars ++ stes ++ prefix ++ stars

symTabToString' :: Scope -> Int -> String
symTabToString' scope n = 
    let
        stars = "********************************************\n"
        prefix = indent n
        intro = "Printing Scope:\n"
        entries = "keys: " ++  (show (M.keys scope)) ++ "\n"
        stes = getSTEs (M.elems scope) (n+1)
    in
        prefix ++ entries ++ prefix ++ intro ++ prefix ++ stars ++ stes ++ prefix ++ stars

-- Helper method for symTabToString to get the
-- STEs in the symbol table for printing.
getSTEs :: [STE] -> Int -> String
getSTEs ((ClassSTE cname scope coffset):rest) n =
    let 
        prefix = indent n
        info = "ClassSTE: " ++ cname ++ ": max_offset = " ++ show coffset ++ "\n"
        entries = symTabToString' scope (n+1)
        everythingElse = getSTEs rest (n)
    in
        prefix ++ info ++ entries ++ everythingElse

getSTEs ((MethodSTE mname (scope, tsig) moffset):rest) n =
    let 
        prefix = indent n
        info = "MethodSTE: " ++ mname ++ ": max_offset = " ++ show moffset ++ "\n"
        signature = "Signature = " ++ (show tsig) ++ "\n"
        entries = symTabToString' scope (n+1)
        everythingElse = getSTEs rest (n)
    in
        prefix ++ info ++ prefix ++ signature ++ entries ++ everythingElse

getSTEs ((VarSTE vtype vname base offset):rest) n =
    let 
        prefix = indent n
        info = "VarSTE: " ++ (show vname) ++ ":\n"
        tp = "Type: " ++ (show vtype) ++ "\n"
        baseOffset = "Base: " ++ base ++ ",\tOffset: " ++ (show offset) ++ "\n"
        everythingElse = getSTEs rest (n)
    in
        prefix ++ info ++ prefix ++ tp ++ prefix ++ baseOffset ++ everythingElse

getSTEs [] n = ""

-- Indent method to provide spacing for printing
-- the STE.
indent :: Int -> String
indent 0 = ""
indent n = "\t" ++ indent (n-1)

-- ========================= Testing
-- Testing
checkExpect:: (Eq a) => (Show a) => a -> a -> String
checkExpect check expect =
    if check /= expect
    then "checkExpect FAILED, got: "
         ++ (show check) ++ ", expected: " ++ (show expect)
    else "checkExpect PASSED"