-- Parser.hs
--
-- Step (3) of compiling.
--
-- Parser module takes the list of (Token, location) tuples generated by Lexer.hs and Relex.hs,
-- and generates an AST
--

module Parser where

import Lexer
import Util

-- entry point
genAST :: [(Token, (Int,Int))] -> AST
genAST ((t, (row,col)):rest) = 
    if checkTokens ((t, (row,col)):rest) then 
        parseProg ((t, (row,col)):rest)
    else
        Epsilon

-- looks for UnexpecedEOF to find unclosed block comments
checkTokens :: [(Token, (Int,Int))] -> Bool
checkTokens [] = True
checkTokens ((TokenEOF, (row,col)):rest) = True
checkTokens ((UnexpectedEOF msg, (row,col)):rest) = 
    error ("ERR: (Parser - CheckTokens) Check that all block comments are closed near [" ++ show row ++ ", " ++ show col ++ "]\n") 
checkTokens (t:rest) = checkTokens rest

-- ParentNode of the entire AST
parseProg :: [(Token, (Int,Int))] -> AST
parseProg ts =
    let
        ts1                  = match ts TokenMeggyImport
        (main_class, ts2)    = parseMC ts1
        other_classes        = parseCD ts2
    in
        (Prog main_class other_classes)

-- MainClass
parseMC :: [(Token, (Int,Int))] -> (AST, [(Token, (Int,Int))])
parseMC ((TokenClass, (row,col)):(TokenID class_name, (row1,col1)):rest) = 
    let
        ts                  = match rest TokenLeftCurly
        ts1                 = match ts   TokenPublic
        ts2                 = match ts1  TokenStatic
        ts3                 = match ts2  TokenVoid
        ts4                 = match ts3  TokenMain
        ts5                 = match ts4  TokenLeftParen
        ts6                 = match ts5  TokenString
        ts7                 = match ts6  TokenLeftBracket
        (argsname:ts8)      = match ts7  TokenRightBracket
        ts9                 = match ts8  TokenRightParen
        ts10                = match ts9  TokenLeftCurly
        (main_method, ts11) = parseSL ts10
        ts12                = match ts11 TokenRightCurly
        ts13                = match ts12 TokenRightCurly
        --                  Token right curly is handled by the FOLLOW sets in the parseStm grammar
    in
        (MainClass main_method, ts13)

-- VariableDeclaration
parseVD :: [(Token, (Int,Int))] -> (AST, [(Token, (Int,Int))])
parseVD ts1 = 
    let
        (vars, ts2) = parseVD' ts1
    in
        (VarDecl vars, ts2)

parseVD' :: [(Token, (Int,Int))] -> ([AST], [(Token, (Int,Int))])
parseVD' ts
    | "FOLLOW" == vname && VoidType == vtype = ([], ts1)
    | otherwise =
        let
            var1 = Variable vtype vname
            ts2  = match ts1 TokenSemiColon
            (var2, ts3) = parseVD' ts2
        in
            (var1:var2, ts3)
    where
        ((vname, vtype), ts1) = parseU ts
        
-- ClassDeclaration
parseCD :: [(Token, (Int,Int))] -> [AST]
parseCD ((TokenEOF, (row,col)):rest) = []
parseCD ((TokenClass, (row,col)):(TokenID class_name, (row1,col1)):rest) = 
    let
        ts1                    = match rest TokenLeftCurly
        (variableDecl, ts2)    = parseVD ts1
        (list_of_methods, ts3) = parseMD ts2
        ts4                    = match ts3 TokenRightCurly
    in
        (Class variableDecl (MethDecl list_of_methods) class_name):(parseCD ts4)
parseCD ((t, (row,col)):rest) = error ("ERR: (Parser - ParseCD) Invalid class declaration on token " ++ show t ++ " near [" ++ show row ++ ", " ++ show col ++ "]\n")

-- MethodDeclaration
parseMD :: [(Token, (Int,Int))] -> ([AST], [(Token, (Int,Int))])
parseMD all@((TokenRightCurly, (row,col)):rest) = ([], all)
parseMD ((TokenPublic, (row,col)):(return, (row1,col1)):(TokenID method_name, (row2,col2)):rest) =
    let
        return_type             = grab_Type return
        ts1                     = match rest TokenLeftParen
        (params, ts2)           = parseParamDecl ts1
        ts3                     = match ts2 TokenLeftCurly
        (variableDecl, ts4)     = parseVD ts3
        (methodBody, ts5)       = parseSL ts4
        ts6@((t, (row,col)):remain) = match ts5 TokenRightCurly
    in
        if t == TokenPublic then
            let
                (astList, stuff) = parseMD ts6
            in
                (((Method variableDecl methodBody method_name (TS params return_type)):astList), stuff)
        else
            (((Method variableDecl methodBody method_name (TS params return_type)):[]), ts6)

parseMD ((notPublic, (row,col)):(return, (row1,col1)):(TokenID method_name, (row2,col2)):rest) =
    error ("ERR: (Parser - ParseMD) Public keyword not found for method starting, on token " ++ show notPublic ++ " at [" ++ show row ++ ", " ++ show col ++ "]\n")

parseMD ((TokenPublic, (row,col)):(return, (row1,col1)):(notID, (row2,col2)):rest) =
    error ("ERR: (Parser - ParseMD) Identifier not found for method starting, on token " ++ show notID ++ " at [" ++ show row2 ++ ", " ++ show col2 ++ "]\n")

parseMD ((t1, (row,col)):rest) =
    error ("ERR: (Parser - ParseMD) for method starting at " ++ show t1 ++ " at [" ++ show row ++ ", " ++ show col ++ "]\n")

-- Return expression
parseZ :: [(Token, (Int,Int))] -> (AST, [(Token, (Int,Int))])
parseZ ts@((t, (row,col)):rest) =
    if first_Expression t then
        let
            (returnAst, ts1) = parseE ts
            ts2 = match ts1 TokenSemiColon 
        in
            (Return returnAst, ts2)
    else
        error ("ERR: (Parser - ParseZ) Return declaration error on token " ++ show t ++ " at [" ++ show row ++ ", " ++ show col ++ "]\n")

-- Method Declaration Grammar
parseParamDecl :: [(Token, (Int,Int))] -> ([(String, Type)], [(Token, (Int,Int))])
parseParamDecl ts@((t, (row,col)):rest) =
    if first_Type t then
        let
            (ts1, rest1) = parseU ts
            (ts2, rest2) = parseV rest1
        in
            ((ts1:ts2), rest2)
    else if t == TokenRightParen then
        ([], rest)
    else
        error ("ERR: (Parser - ParseParamDecl) Invalid prameter declaration in method on token " ++ show t ++ " at [" ++ show row ++ ", " ++ show col ++ "]\n")

parseU :: [(Token, (Int,Int))] -> ((String, Type), [(Token, (Int,Int))])
parseU ((typeTok, (row,col)):((TokenID name), (r1,c1)):rest) =
    if first_Type typeTok then
        ((name, (grab_Type typeTok)), rest)
    else
        error ("ERR: (Parser - ParseU) on token " ++ show typeTok ++ " at [" ++ show row ++ ", " ++ show col ++ "]\n")

parseU ts@((t, (row,col)):rest) =
    if follow_VD t then
        (("FOLLOW", VoidType), ts)
    else
        error ("ERR: (Parser - ParseU) on token " ++ show t ++ " at [" ++ show row ++ ", " ++ show col ++ "]\n")

parseV :: [(Token, (Int,Int))] -> ([(String, Type)], [(Token, (Int,Int))])
parseV ts@((t, (r,c)):rest)
    | TokenRightParen == t = ([], rest)
    | otherwise =
        let
            ts0 = match ts TokenComma
            (ts1, rest1) = parseU ts0
            (ts2, rest2) = parseV rest1
        in
            ((ts1:ts2), rest2)

--Statement Grammar, SL is a Body AST
parseSL :: [(Token, (Int,Int))] -> (AST, [(Token, (Int,Int))])
parseSL tokenList = 
    let
        (sl, remainingTokens) = parseSL' tokenList
    in
        (Body sl, remainingTokens)

-- Helper function for parseSL to recurse on each statement in a statement list
parseSL' :: [(Token, (Int,Int))] -> ([AST], [(Token, (Int,Int))])
parseSL' tokenList
    | Epsilon == stmt1 = ([], ts1)
    | otherwise =
        let
            (stmt2, ts2) = parseSL' ts1
        in
            (stmt1:stmt2, ts2)
    where 
        (stmt1, ts1) = parseStm tokenList

parseStm :: [(Token, (Int,Int))] -> (AST, [(Token, (Int,Int))])
parseStm all@((TokenRightCurly, (row,col)):rest) = (Epsilon, all)

parseStm ((TokenReturn, (row,col)):rest) = parseZ rest

parseStm ((TokenLeftCurly , (row,col)):rest) = 
    let
        (body_ast, ts1) = parseSL' rest
        ts2             = match ts1 TokenRightCurly
    in
        (Body body_ast, ts2)

parseStm ((TokenIf, (row,col)):rest) = 
    let 
        ts2              = match rest TokenLeftParen
        (condition, ts3) = parseE ts2
        ts4              = match ts3  TokenRightParen
        (stms, ts5)      = parseStm ts4
        (elseOption, ts6)= parseA ts5
    in
        ((If condition stms elseOption), ts6)

parseStm ((TokenWhile, (row,col)):rest) = 
    let 
        ts2              = match rest TokenLeftParen
        (condition, ts3) = parseE ts2
        ts4              = match ts3  TokenRightParen
        (stms, ts5)      = parseStm ts4
    in
        ((While condition stms), ts5)

parseStm ((TokenMeggySetPix, (row,col)):rest) = 
    let 
        ts1              = match rest TokenLeftParen
        (exp1, ts2)      = parseE ts1
        ts3              = match ts2  TokenComma
        (exp2, ts4)      = parseE ts3
        ts5              = match ts4  TokenComma
        (exp3, ts6)      = parseE ts5
        ts7              = match ts6  TokenRightParen
        ts8              = match ts7  TokenSemiColon
    in
        ((SetPixel exp1 exp2 exp3), ts8)

parseStm ((TokenMeggyDelay, (row,col)):rest) = 
    let 
        ts1              = match rest TokenLeftParen
        (exp1, ts2)      = parseE ts1
        ts3              = match ts2  TokenRightParen
        ts4              = match ts3  TokenSemiColon
    in
        ((Delay exp1), ts4)

parseStm ((TokenMeggyToneStart, (row,col)):rest) = 
    let 
        ts1              = match rest TokenLeftParen
        (exp1, ts2)      = parseE ts1
        ts3              = match ts2  TokenComma
        (exp2, ts4)      = parseE ts3
        ts5              = match ts4  TokenRightParen
        ts6              = match ts5  TokenSemiColon
    in
        ((ToneStart exp1 exp2), ts6)

parseStm ((TokenNew, (row,col)):(TokenID id, (r1,c1)):rest) = 
    let
        ts1 = match rest TokenLeftParen
        ts2 = match ts1  TokenRightParen
        ts3 = match ts2  TokenDot
        (invocation, ts4) = parseInvoke ts3 --This should an invocation AST
        ts5          = match ts4 TokenSemiColon
    in
        (Instance invocation id, ts5)

parseStm ((TokenThis, (row,col)):rest) = 
    let
        ts1 = match rest TokenDot
        (invocation, ts2) = parseInvoke ts1 --This should an invocation AST
        ts3 = match ts2 TokenSemiColon
    in
        (Instance invocation "this", ts3)

parseStm ((t, (row,col)):rest) = 
    error ("ERR: (Parser - ParseStm) Invalid instance on token string " ++ show t ++ "... starting at [" ++ show row ++ ", " ++ show col ++ "]\n")

-- for else body of if-else
parseA  :: [(Token, (Int,Int))] -> (AST, [(Token, (Int,Int))])
parseA ((TokenElse, (row,col)):rest) = 
    let
        (stmt, ts1)    = parseStm rest
    in
        (stmt, ts1)

parseA ((t, (row,col)):rest) = 
    if t == TokenRightCurly || t == TokenIf || t == TokenWhile || t == TokenMeggySetPix || t == TokenMeggyDelay then
        (Epsilon, ((t, (row,col)):rest))
    else 
        error("ERR: (Parser - ParseA) Invalid else body on token " ++ show t ++ " at [" ++ show row ++ ", " ++ show col ++ "]\n")

--Expression Grammar
parseE  :: [(Token, (Int,Int))] -> (AST, [(Token, (Int,Int))])
parseE ((t, (row,col)):rest) =
    if first_Expression t then
        let
            (f_ast, ts1) = parseF ((t, (row,col)):rest)
        in
            parseE' ts1 f_ast
    else
        error ("ERR: (Parser - ParseE) Invalid expression on token " ++ show t ++ " at [" ++ show row ++ ", " ++ show col ++ "]\n")

parseE' :: [(Token, (Int,Int))] -> AST -> (AST, [(Token, (Int,Int))])
parseE' ((TokenAnd, (row,col)):rest) child =
    let 
        (f_ast, ts1)      = parseF rest
    in
        parseE' ts1 (LogicalAnd child f_ast)

parseE' ((t, (row,col)):rest) child = 
    if  follow_E t  then
        (child, ((t, (row,col)):rest))
    else
        error("ERR: (Parser - ParseE') Invalid expression near Logical And on token " ++ show t ++ " at [" ++ show row ++ ", " ++ show col ++ "]\n")

--Fs
parseF  :: [(Token, (Int,Int))] -> (AST, [(Token, (Int,Int))])
parseF ((t, (row,col)):rest) =
    if first_Expression t then
        let
            (g_ast, ts1) = parseG ((t, (row,col)):rest)
        in
            parseF' ts1 g_ast
    else
        error ("ERR: (Parser - ParseF) Invalid expression on token " ++ show t ++ " at [" ++ show row ++ ", " ++ show col ++ "]\n")

parseF' :: [(Token, (Int,Int))] -> AST -> (AST, [(Token, (Int,Int))])
parseF' ((TokenEquality, (row,col)):rest) child =
    let 
        (g_ast, ts1)      = parseG rest
    in
        parseF' ts1 (LogicalEqual child g_ast)

parseF' ((t, (row,col)):rest) child = 
    if follow_F' t then
        (child, ((t, (row,col)):rest))
    else
        error("ERR: (Parser - ParseF') Invalid expression near Logical Equal on token " ++ show t ++ " at [" ++ show row ++ ", " ++ show col ++ "]\n")

--Gs
parseG  :: [(Token, (Int,Int))] -> (AST, [(Token, (Int,Int))])
parseG ((t, (row,col)):rest) =
    if first_Expression t then
        let
            (h_ast, ts1) = parseH ((t, (row,col)):rest)
        in
            parseG' ts1 h_ast
    else
        error ("ERR: (Parser - ParseG) Invalid expression on token " ++ show t ++ " at [" ++ show row ++ ", " ++ show col ++ "]\n")

parseG' :: [(Token, (Int,Int))] -> AST -> (AST, [(Token, (Int,Int))])
parseG' ((TokenLessThan, (row,col)):rest) child =
    let 
        (h_ast, ts1)      = parseH rest
    in
        parseG' ts1 (LessThan child h_ast)

parseG' ((t, (row,col)):rest) child = 
    if follow_G' t then
        (child, ((t, (row,col)):rest))
    else
        error("ERR: (Parser - ParseG') Invalid expression near < on token " ++ show t ++ " at [" ++ show row ++ ", " ++ show col ++ "]\n")

--Hs
parseH  :: [(Token, (Int,Int))] -> (AST, [(Token, (Int,Int))])
parseH ((t, (row,col)):rest) =
    if first_Expression t then
        let
            (i_ast, ts1) = parseI ((t, (row,col)):rest)
        in
            parseH' ts1 i_ast
    else
        error ("ERR: (Parser - ParseH) Invalid expression on token " ++ show t ++ " at [" ++ show row ++ ", " ++ show col ++ "]\n")

parseH' :: [(Token, (Int,Int))] -> AST -> (AST, [(Token, (Int,Int))])
parseH' ((TokenAdd, (row,col)):rest) child =
    let 
        (i_ast, ts1)      = parseI rest
    in
        parseH' ts1 (Add child i_ast)

parseH' ((TokenSub, (row,col)):rest) child =
    let 
        (i_ast, ts1)      = parseI rest
    in
        parseH' ts1 (Sub child i_ast)

parseH' ((t, (row,col)):rest) child = 
    if follow_H' t then
        (child, ((t, (row,col)):rest))
    else
        error("ERR: (Parser - ParseH') Invalid expression near + or - on token " ++ show t ++ " at [" ++ show row ++ ", " ++ show col ++ "]\n")

--Is
parseI  :: [(Token, (Int,Int))] -> (AST, [(Token, (Int,Int))])
parseI ((t, (row,col)):rest) =
    if first_Expression t then
        let
            (j_ast, ts1) = parseJ ((t, (row,col)):rest)
        in
            parseI' ts1 j_ast
    else
        error ("ERR: (Parser - ParseI) Invalid expression on token " ++ show t ++ " at [" ++ show row ++ ", " ++ show col ++ "]\n")

parseI' :: [(Token, (Int,Int))] -> AST -> (AST, [(Token, (Int,Int))])
parseI' ((TokenMul, (row,col)):rest) child =
    let 
        (j_ast, ts1)      = parseJ rest
    in
        parseI' ts1 (Mul child j_ast)

parseI' ((t, (row,col)):rest) child = 
    if follow_I' t then
        (child, ((t, (row,col)):rest))
    else
        error("ERR: (Parser - ParseI') Invalid expression near * on token " ++ show t ++ " at [" ++ show row ++ ", " ++ show col ++ "]\n")

-- --J original
-- parseJ  :: [(Token, (Int,Int))] -> (AST, [(Token, (Int,Int))])
-- parseJ ((TokenNew, (row,col)):(TokenID id, (r1,c1)):(TokenLeftParen, (r2,c2)):(TokenRightParen, (r3,c3)):rest) = 
--     let
--         (invocation, ts1) = parseL rest
--     in
--         (Instance invocation id, ts1)

--J new
parseJ  :: [(Token, (Int,Int))] -> (AST, [(Token, (Int,Int))])
parseJ ((TokenNew, (row,col)):rest) = parseP rest

parseJ ((TokenByteCast, (row,col)):rest) =
    let
        (j_ast, ts1) = parseJ rest
    in
        (ByteCast j_ast, ts1)

parseJ ((t, (row,col)):rest) =
    if first_K t then
        parseK ((t, (row,col)):rest)
    else
        error ("ERR: (Parser - ParseJ) Invalid expression on token " ++ show t ++ " at [" ++ show row ++ ", " ++ show col ++ "]\n")

--P
parseP :: [(Token, (Int,Int))] -> (AST, [(Token, (Int,Int))])
parseP ((TokenID id, (r1,c1)):(TokenLeftParen, (r2,c2)):(TokenRightParen, (r3,c3)):rest) =
    let
        (invocation, ts1) = parseL rest
    in
        (Instance invocation id, ts1)

parseP ((TokenInt, (r1,c1)):(TokenLeftBracket, (r2,c2)):rest) =
    let
        (capacity, ts1) = parseE rest
        ts2 = match ts1 TokenRightBracket
    in
        (IntArrayInstance capacity, ts2)

parseP ((TokenColor, (r1,c1)):(TokenLeftBracket, (r2,c2)):rest) =
    let
        (capacity, ts1) = parseE rest
        ts2 = match ts1 TokenRightBracket
    in
        (ColorArrayInstance capacity, ts2)

--K
parseK  :: [(Token, (Int,Int))] -> (AST, [(Token, (Int,Int))])
parseK ((TokenNot, (row,col)):rest) = 
    let
        (k_ast, ts1) = parseK rest
    in
        (LogicalNot k_ast, ts1)

parseK ((TokenSub, (row,col)):rest) = 
    let
        (j_ast, ts1) = parseJ rest
    in
        (UnaryMinus j_ast, ts1)

parseK ((t, (row,col)):rest) = 
    if first_L t then
        parseL ((t, (row,col)):rest)
    else
        error ("ERR: (Parser - ParseK) on token " ++ show t ++ " at [" ++ show row ++ ", " ++ show col ++ "]\n")

--L
parseL  :: [(Token, (Int,Int))] -> (AST, [(Token, (Int,Int))])
--Value expressions
parseL ((TokenNum x, (row,col)):rest)                                     = (IntLiteral x, rest)
parseL ((TokenMeggyColor, (row,col)) :(TokenColorValue x, (r1,c1)):rest)  = (ColorLiteral x, rest)
parseL ((TokenMeggyButton, (row,col)):(TokenButtonValue x, (r1,c1)):rest) = (ButtonLiteral x, rest)
parseL ((TokenMeggyTone, (row,col))  :(TokenToneValue x, (r1,c1)):rest)   = (ToneLiteral x, rest)
parseL ((TokenTrue, (row,col)):rest)   = (Boolean True, rest)
parseL ((TokenFalse, (row,col)):rest)  = (Boolean False, rest)
parseL ((TokenID id, (row,col)):rest)  = (Identifier id, rest)

--More Complicated expressions
parseL ((TokenThis, (row,col)):rest) = 
    let
        (invocation, ts1) = parseL(rest)
    in
        (Instance invocation "this", ts1)

parseL ((TokenLeftParen, (row,col)):rest) = 
    let
        (exp, ts1)       = parseE rest
        ts2              = match ts1 TokenRightParen
    in
        (ParenExp exp, ts2)

parseL ((TokenDot, (row,col)):rest) = parseInvoke rest

parseL ((TokenMeggyGetPix, (row,col)):rest) = 
    let
        ts1              = match rest TokenLeftParen
        (exp1, ts2)      = parseE ts1
        ts3              = match ts2  TokenComma
        (exp2, ts4)      = parseE ts3
        ts5              = match ts4  TokenRightParen
    in
        (GetPixel exp1 exp2, ts5)

parseL ((TokenMeggyCheckButton, (row,col)):rest) = 
    let
        ts1              = match rest TokenLeftParen
        (exp, ts2)       = parseE ts1
        ts3              = match ts2  TokenRightParen
    in
        (CheckButton exp, ts3)

parseL ts@((t, (row,col)):rest) = 
    if first_Expression t then
        let
            (exp, ts1) = parseE ts
        in
            parseB ts1 exp
    else 
        error("Parsing Error in parseL on token " ++ show t ++ " at [" ++ show row ++ ", " ++ show col ++ "]\n")

-- parseL is ambiguous between E[E] and E.length, 
parseB ::  [(Token, (Int,Int))] -> AST -> (AST, [(Token, (Int,Int))])
parseB ((TokenLeftBracket, (row,col)):rest) array = 
    let
        (index, ts1) = parseE rest
        ts2        = match ts1 TokenRightBracket
    in
        (ArrayAccess array index, ts2)

parseB ((TokenDot, (row, col)):rest) array = 
    let
        ts1 = match rest TokenLength
    in
        (ArrayLength array, ts1)

-- Method Invocation Grammar
parseInvoke :: [(Token, (Int,Int))] -> (AST, [(Token, (Int,Int))])
parseInvoke ((TokenID id, (row,col)):rest) = 
    let
        ts1 = match rest TokenLeftParen
        (params, ts2) = parseParam ts1
        -- TokenRightParen matched by parseParam
    in
        (Invoke params id, ts2)

parseParam :: [(Token, (Int,Int))] -> ([AST], [(Token, (Int,Int))])
parseParam ((t, (row,col)):rest) =
    if first_Expression t then
        let
            (exp1, ts1) = parseX ((t, (row,col)):rest)
            (exp2, ts2) = parseY (ts1)
        in
            ((exp1:exp2), ts2)
    else if t == TokenRightParen then
        ([], rest)
    else
        error ("ERR: (Parser - ParseParam) on token " ++ show t ++ " at [" ++ show row ++ ", " ++ show col ++ "]\n")

parseX :: [(Token, (Int,Int))] -> (AST, [(Token, (Int,Int))])
parseX tokens = parseE tokens

parseY :: [(Token, (Int,Int))] -> ([AST], [(Token, (Int,Int))])
parseY ts@((t, (r,c)):rest)
    | TokenRightParen == t = ([], rest)
    | otherwise =
        let
            ts0    = match ts TokenComma
            (ts1, rest1) = parseX ts0
            (ts2, rest2) = parseY rest1
        in
            ((ts1:ts2), rest2)

-- match expected token, otherwise throw exception
-- Works for all tokens that don't have extra data associated with them.
match :: [(Token, (Int,Int))] -> Token -> [(Token, (Int,Int))]
match ((given, (row,col)):rest) expected =
    if given == expected then 
        rest
    else error (   "ERR: (Parser - match)" 
                ++ " ["++(show row)++", " ++ (show col) ++ "]"
                ++ "\n\t -> Expected " ++ (show expected) ++ " but found " ++ (show given) ++ "\n")

-- FIRST and FOLLOW sets from predictive parser
follow_VD :: Token -> Bool
follow_VD t =
    if t == TokenPublic then          -- in Class Declaration
        True
    else if t == TokenRightCurly then -- in Class Declaration
        True
    else if first_Stm t then          -- in Method Declaration
        True
    else if t == TokenReturn then     -- in Method Declaration
        True
    else
        False

first_Stm :: Token -> Bool
first_Stm t =
    case t of
        TokenIf             -> True
        TokenWhile          -> True
        TokenLeftCurly      -> True
        TokenMeggySetPix    -> True
        TokenMeggyToneStart -> True
        TokenMeggyDelay     -> True
        TokenMeggySetAux    -> True
        TokenNew            -> True
        TokenByteCast       -> True
        _                   -> first_K t


first_Expression :: Token -> Bool
first_Expression t = 
    case t of 
        TokenMeggyColor       -> True
        TokenMeggyButton      -> True
        TokenMeggyTone        -> True
        TokenMeggyGetPix      -> True
        TokenMeggyCheckButton -> True
        TokenDot       -> True
        TokenLeftParen -> True
        TokenNum _     -> True
        TokenTrue      -> True
        TokenFalse     -> True
        TokenThis      -> True
        TokenID _      -> True
        TokenNot       -> True
        TokenSub       -> True
        TokenNew       -> True
        TokenByteCast  -> True
        _ -> False

first_K :: Token -> Bool
first_K t =
    case t of 
        TokenMeggyColor  -> True
        TokenMeggyButton -> True
        TokenMeggyTone   -> True
        TokenMeggyGetPix -> True
        TokenMeggyCheckButton -> True
        TokenDot              -> True
        TokenLeftParen        -> True
        TokenNum _ -> True
        TokenTrue  -> True
        TokenFalse -> True
        TokenThis  -> True
        TokenID _  -> True
        TokenNot   -> True
        TokenSub   -> True
        _ -> False

first_L :: Token -> Bool
first_L t =
    case t of 
        TokenMeggyColor  -> True
        TokenMeggyButton -> True
        TokenMeggyTone   -> True
        TokenMeggyGetPix -> True
        TokenMeggyCheckButton -> True
        TokenLeftParen        -> True
        TokenDot   -> True
        TokenNum _ -> True
        TokenTrue  -> True
        TokenFalse -> True
        TokenThis  -> True
        TokenID _  -> True
        _ -> False

first_Type :: Token -> Bool
first_Type t =
    case t of
        TokenBoolean -> True
        TokenByte    -> True
        TokenInt     -> True
        TokenVoid    -> True
        TokenMeggyButtonType -> True
        TokenMeggyColorType  -> True
        TokenMeggyToneType   -> True
        TokenIntArrayType    -> True
        TokenColorArrayType  -> True
        _ -> False

grab_Type :: Token -> Type
grab_Type t =
    case t of
        TokenBoolean -> BooleanType
        TokenByte    -> ByteType
        TokenInt     -> IntType
        TokenVoid    -> VoidType
        TokenMeggyButtonType -> MeggyButtonType
        TokenMeggyColorType  -> MeggyColorType
        TokenMeggyToneType   -> MeggyToneType
        TokenIntArrayType    -> IntArrayType
        TokenColorArrayType  -> ColorArrayType
        _ -> error ("ERR: (Parser - grab_Type) Did not expect " ++ show t ++ "as a type \n")

follow_E :: Token -> Bool
follow_E  t = 
    case t of 
        TokenComma -> True
        TokenDot   -> True
        TokenRightParen -> True
        TokenSemiColon  -> True
        TokenRightBracket -> True
        _  -> False

follow_F' :: Token -> Bool
follow_F' t = (follow_E t) || t == TokenAnd

follow_G' :: Token -> Bool
follow_G' t = (follow_F' t) || t == TokenEquality

follow_H' :: Token -> Bool
follow_H' t = (follow_G' t) || t == TokenLessThan

follow_I' :: Token -> Bool
follow_I' t = (follow_H' t) || t == TokenAdd || t == TokenSub
