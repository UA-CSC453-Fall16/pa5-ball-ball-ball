
-- TypeCheck.hs
--
-- Step (5) of compiling.
--
-- TypeCheck module takes the AST generated by Parser.hs and the symbol table
-- generated by BuildST.hs and checks the types of all AST nodes
--

module TypeCheck where

import Util
import SymbolTable

-- entry point
typeCheck :: (AST, SymbolTable)-> Bool
typeCheck (root, st) 
    | programType == VoidType = True 
    | otherwise    = error("Invalid program type returned: " ++ show programType)
    where
        programType = tCheck (root, st)


tCheck :: (AST, SymbolTable) -> Type
{-
------- Literal AST Nodes
-}
tCheck ((IntLiteral value), st)    = IntType
tCheck ((ColorLiteral value), st)  = MeggyColorType
tCheck ((ButtonLiteral value), st) = MeggyButtonType
tCheck ((ToneLiteral value), st)   = MeggyToneType
tCheck ((Boolean value), st)       = BooleanType
tCheck (Epsilon, st)               = VoidType


{-
------- Top level n-ary AST nodes 
-}
tCheck ((Prog main (aClass:other_classes) ), st)
    | mainType  /= VoidType = error("Invalid type found in main class: " ++ (show mainType))
    | childType /= VoidType = error("Invalid type found in class " ++ class_name ++ ": " ++ (show childType))
    | otherwise  = tCheck ((Prog Epsilon other_classes), st) 
    where
        mainType  = tCheck (main, st)
        childType = tCheck (aClass, st)

tCheck ((Prog main [] ), st)
    | mainType  /= VoidType = error("Invalid type found in main class: " ++ (show mainType))
    | otherwise  = VoidType
    where
        mainType  = tCheck (main, st)

tCheck ((Class instVars methods class_name), st)
    | classType  /= VoidType = error("Invalid type found in class " ++ class_name ++": " ++ (show classType))
    | otherwise   = VoidType
    where
        st1 = pushScope st class_name
        classType = tCheck (methods, st1)
        st2 = popScope st1

tCheck ((MainClass child), st)
    | actual_type /= VoidType = error("Invalid type found in main: " ++ (show actual_type))
    | otherwise    = VoidType
    where
        actual_type = tCheck (child, st)

tCheck ((Method variables body method_name (TS params return_type)), st)
    | actual_type == ByteType && return_type == IntType = VoidType
    | actual_type /= return_type = error("Invalid type found in main: " ++ (show actual_type))
    | otherwise    = VoidType
    where
        st1         = pushScope st method_name
        actual_type = tCheck (body, st1)
        st2         = popScope st1

tCheck ((Body []), st)            = VoidType
tCheck ((Body (Epsilon:[]) ), st) = VoidType
tCheck ((Body ((Return ret_exp):[])), st) = tCheck ((Return ret_exp), st)
tCheck ((Body (child:rest)), st) =
    let
        typeBody = tCheck (child, st) 
    in
        tCheck ((Body rest), st)

tCheck ((MethDecl []), st) = VoidType
tCheck ((MethDecl (left:rest)), st)
    | methodType /= TypeVoid = error ("Type check error, method with bad type\n")
    | otherwise = tCheck ((MethDecl rest), st)
    where
        methodType = tCheck (left, st)

tCheck (VarDecl _, st) = VoidType

tCheck ((Return child), st@(SymTab scope [method_name,class_name])) = 
    let
        signatureRetType = getReturn st
        returnExpType = tCheck (child, st)
    in
        if signatureRetType == VoidType then
            error("Error: " ++ method_name ++ " has a return type of void, but contains a return expression")
        else if returnExpType == signatureRetType then
            returnExpType
        else if returnExpType == ByteType && signatureRetType == IntType then
            ByteType
        else
            error(method_name ++ ": Return expression type: " ++ show returnExpType ++ ", does not match signature return type: " ++ show signatureRetType)

tCheck ((Identifier name), st) =  lookupParamType st name 

{-
------- Unary AST Nodes
-}
tCheck ((Delay child), st)  --Check for negative child
    | stmType  == IntType = VoidType 
    | otherwise = error("Invalid arg type for delay: " ++ (show stmType) ++ ", expected IntType")
    where
        stmType = tCheck (child, st)
        random = error (show st ++ ",\n ast: " ++ show child ) 

tCheck ((CheckButton child), st)   
    | expType  == MeggyButtonType = BooleanType
    | otherwise = error("Invalid arg type for checkButton: " ++ (show expType) ++ ", expected MeggyButtonType")
    where
        expType = tCheck (child, st)

tCheck ((LogicalNot child), st)    
    | expType  == BooleanType = BooleanType
    | otherwise = error("Invalid type found in operand of logical negation: " ++ (show expType) ++ ", expected BooleanType")
    where
        expType = tCheck (child, st)

tCheck ((UnaryMinus child), st)    
    | expType  == IntType  = IntType
    | expType  == ByteType = ByteType
    | otherwise = error("Invalid type found in operand of arithmetic negation (-): " ++ (show expType) ++ ", expected ByteType or IntType")
    where
        expType = tCheck (child, st)

tCheck ((ByteCast child), st) 
    | expType  == IntType  = ByteType
    | expType  == ByteType = ByteType
    | otherwise = error("ByteCast expected operand of type Int or Byte but it was " ++ (show expType))
    where
        expType = tCheck (child, st)

tCheck ((ParenExp child), st) 
    | expType  == VoidType = error("Parenthesis wrap unexpected type (usually statement type): " ++ (show expType))
    | otherwise = expType
    where
        expType = tCheck (child, st)

tCheck ((SetAuxLEDs child), st) 
    | expType  /= IntType = error("Type Error: Invalid expression type passed to Meggy.setAuxLEDs(): " ++ (show expType))
    | otherwise = expType
    where
        expType = tCheck (child, st)

{-
------- Binary AST nodes
-}
tCheck ((GetPixel child1 child2), st) 
    | exp1Type /= ByteType  = error("SetPixel expected first arg of type Byte but it was " ++ (show exp1Type))
    | exp2Type /= ByteType  = error("SetPixel expected second arg of type Byte but it was " ++ (show exp2Type))
    | otherwise = MeggyColorType
    where
        exp1Type = tCheck (child1, st)
        exp2Type = tCheck (child2, st)

tCheck ((ToneStart child1 child2), st) 
    | exp1Type /= MeggyToneType  = error("SetPixel expected first arg of type Byte but it was " ++ (show exp1Type))
    | exp2Type /= IntType        = error("SetPixel expected second arg of type Byte but it was " ++ (show exp2Type))
    | otherwise = VoidType
    where
        exp1Type = tCheck (child1, st)
        exp2Type = tCheck (child2, st)

tCheck ((LogicalAnd child1 child2), st)
    | exp1Type /= BooleanType = error("Logical And (&&) has invalid type for left operand " ++ (show exp1Type))
    | exp2Type /= BooleanType = error("Logical And (&&) has invalid type for right operand " ++ (show exp2Type))
    | otherwise = BooleanType
    where
        exp1Type = tCheck (child1, st)
        exp2Type = tCheck (child2, st)

tCheck ((LessThan child1 child2), st)
    | exp1Type == IntType && exp2Type == ByteType  = BooleanType
    | exp1Type == ByteType && exp2Type == IntType  = BooleanType
    | exp1Type == exp2Type && exp1Type == ByteType = BooleanType
    | exp1Type == exp2Type && exp1Type == IntType  = BooleanType
    | otherwise = error("LessThan operator (<) has unexpected operand types: " ++ (show exp1Type) ++ " and " ++ (show exp2Type))
    where
        exp1Type = tCheck (child1, st)
        exp2Type = tCheck (child2, st)

tCheck ((LogicalEqual child1 child2), st) 
    | exp1Type == VoidType = error("Logical Equal (==) has invalid type for left operand " ++ (show exp1Type))
    | exp2Type == VoidType = error("Logical Equal (==) has invalid type for right operand " ++ (show exp2Type))
    | exp1Type == MeggyButtonType = error("Logical Equal (==) has invalid type for left operand " ++ (show exp1Type))
    | exp2Type == MeggyButtonType = error("Logical Equal (==) has invalid type for left operand " ++ (show exp1Type))
    | otherwise = BooleanType
    where
        exp1Type = tCheck (child1, st)
        exp2Type = tCheck (child2, st)

tCheck ((Add child1 child2), st)
    | exp1Type == IntType && exp2Type == ByteType  = IntType
    | exp1Type == ByteType && exp2Type == IntType  = IntType
    | exp1Type == exp2Type && exp1Type == ByteType = ByteType
    | exp1Type == exp2Type && exp1Type == IntType  = IntType
    | otherwise = error("Add operator (+) has unexpected operand types: " ++ (show exp1Type) ++ " and " ++ (show exp2Type))
    where 
        exp1Type = tCheck (child1, st)
        exp2Type = tCheck (child2, st)

tCheck ((Sub child1 child2), st) 
    | exp1Type == IntType && exp2Type == ByteType  = IntType
    | exp1Type == ByteType && exp2Type == IntType  = IntType
    | exp1Type == exp2Type && exp1Type == ByteType = ByteType
    | exp1Type == exp2Type && exp1Type == IntType  = IntType
    | otherwise = error("Sub operator (-) has unexpected operand types: " ++ (show exp1Type) ++ " and " ++ (show exp2Type))
    where
        exp1Type = tCheck (child1, st)
        exp2Type = tCheck (child2, st)

tCheck ((Mul child1 child2), st) 
    | exp1Type == exp2Type && exp1Type == ByteType = IntType
    | otherwise = error("Mul operator (*) has unexpected operand types: " ++ (show exp1Type) ++ " and " ++ (show exp2Type))
    where
        exp1Type = tCheck (child1, st)
        exp2Type = tCheck (child2, st)

tCheck ((While child1 child2), st) 
    | exp1Type /= BooleanType = error("Condition in while loop has unexpected type: " ++ (show exp1Type) ++ " expected BooleanType")
    | exp2Type /= VoidType = error("Body of while loop has unexpected type: " ++ (show exp2Type) ++ " expected Statement Type")
    | otherwise = VoidType
    where
        exp1Type = tCheck (child1, st)
        exp2Type = tCheck (child2, st)

tCheck ((Instance className), st) = ClassType className

-- tCheck ((Invoke receiver params@(first:rest) method_name), st@(SymTab scope (mname:class_name:[]))) =
--     | return
--     |
--     where
--         receiver_returnType   = tCheck (receiver, st)
--         (TS expectedType ret_type) = lookupTypeSig st class_name method_name

-- tCheck ((Invoke receiver params@(first:rest) method_name), st@(SymTab scope (mname:class_name:[]))) =
--     where
--         receiver_type = tCheck (receiver, st)
--         (mname, mscope, (TS params return_type, maxoffset) = namedScopeLookup' scope method_name


-- I'm lost. 
-- Check that method_name belongs to the receiver_returnType class otherwise report error. 



-- tCheck ((Instance child class_name), st) = 
--     if class_name == "this" then
--             tCheck (child, st)
--     else
--         let 
--             st1 = setProgScope st
--             st2 = pushScope st1 class_name
--             return_type = tCheck (child, st2)
--             st3 = popScope st2
--         in
--             return_type

-- --We can assume that the return type declared is the type of this expression or statement
-- st -> (SymTab scope (firstThingInTheListWhichShouldBeTheMethodNameButWeAlreadyHaveThatFromTheLeftPatternMatch:class_name:emptyListHopefullyButInAnErrorCaseItMayNotBeTheEmptyListAndMayCauseAnErrorSoLetsHopeItIsJustTheEmptyList)) ) = 
tCheck ((Invoke recexp params@(param:rest) method_name), st@(SymTab scope [mname,class_name]) ) = 
    let
        receiver_type = tCheck (recexp, st) -- get class of receiver
        (TS expected_types ret_type) = lookupTypeSig st return_type method_name -- verify receiver has method of method_name
    in
        typeCheckInvoke (params, expected_types, ret_type) method_name st

tCheck ((Invoke recexp [] method_name), ts@(SymTab scope [class_name]) ) = 
    let
        receiver_type = tCheck (recexp, st) -- get class of receiver
        (TS expected_types ret_type) = lookupTypeSig ts class_name method_name -- verify receiver has method of method_name
    in
        if length expected_types == 0 then
            ret_type
        else
            error(method_name ++ " invocation error: insufficient number of parameters, requires: " ++ show expected_types)


{-
------- Trinary AST nodes
-}
tCheck ((If child1 child2 child3), st) 
    | exp1Type /= BooleanType = error("Condition of if statement has unexpected type: " ++ (show exp1Type) ++ " expected BooleanType")
    | exp2Type /= VoidType = error("If body has unexpected type: " ++ (show exp2Type) ++ " expected Statement type")
    | exp3Type /= VoidType = error("Else body has unexpected type: " ++ (show exp3Type) ++ " expected Statement type")
    | otherwise = VoidType
    where
        exp1Type = tCheck (child1, st)
        exp2Type = tCheck (child2, st)
        exp3Type = tCheck (child3, st)

tCheck ((SetPixel child1 child2 child3), st)
    | exp1Type /= ByteType  = error("SetPixel expected first arg of ByteType but it was " ++ (show exp1Type))
    | exp2Type /= ByteType  = error("SetPixel expected second arg of ByteType but it was " ++ (show exp2Type))
    | exp3Type /= MeggyColorType = error("SetPixel expected third arg of MeggyColorType but it was " ++ (show exp3Type))
    | otherwise = VoidType
    where
        exp1Type = tCheck (child1, st)
        exp2Type = tCheck (child2, st)
        exp3Type = tCheck (child3, st)

tCheck (ast, st) = 
    error ("ERR: (Type Checking)"
        ++ "\n\t -> Failed to pattern match this AST "
        ++ "\n\t\t -> " ++ show ast 
        ++ "\n\t -> with this symbol table "
        ++ "\n\t\t -> " ++ show st ++ " in type checking")


typeCheckInvoke :: ([AST], [(String,Type)], Type) -> String -> SymbolTable -> Type
typeCheckInvoke ([], []      , ret_type) method_name st = ret_type
typeCheckInvoke (extra:[], [], ret_type) method_name st = error(method_name ++ " invocation error: extra parameter found of type: " ++ show extra)
typeCheckInvoke ([], extra:[], ret_type) method_name st = error(method_name ++ " invocation error: insufficient number of parameters, requires: " ++ show extra)

typeCheckInvoke ( (param@(Identifier id):restP), ((exp_name,expected_type):restE), ret_type) method_name st
    | expected_type == IntType && param_type == ByteType = typeCheckInvoke (restP, restE, ret_type) method_name st1
    | param_type /= expected_type = error(method_name ++ " invocation error: expected type:" ++ show expected_type ++ " but found type: " ++ show param)
    | otherwise = typeCheckInvoke (restP, restE, ret_type) method_name st1
    where
        st1 = pushScope st method_name
        param_type = (tCheck (param, st1))

typeCheckInvoke ( (param:restP), ((exp_name,expected_type):restE), ret_type) method_name st
    | expected_type == IntType && param_type == ByteType = typeCheckInvoke (restP, restE, ret_type) method_name st
    | param_type /= expected_type = error(method_name ++ " invocation error: expected type:" ++ show expected_type ++ " but found type: " ++ show param)
    | otherwise = typeCheckInvoke (restP, restE, ret_type) method_name st
    where
        param_type = (tCheck (param, st))


isReturn :: AST -> Bool
isReturn (Return _) = True
isReturn _          = False
