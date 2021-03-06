{-
                < View in "Raw" on GitHub >
██████╗  █████╗ ██████╗ ███████╗███████╗██████╗    ██╗  ██╗███████╗
██╔══██╗██╔══██╗██╔══██╗██╔════╝██╔════╝██╔══██╗   ██║  ██║██╔════╝
██████╔╝███████║██████╔╝███████╗█████╗  ██████╔╝   ███████║███████╗
██╔═══╝ ██╔══██║██╔══██╗╚════██║██╔══╝  ██╔══██╗   ██╔══██║╚════██║
██║     ██║  ██║██║  ██║███████║███████╗██║  ██║██╗██║  ██║███████║
╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝╚═╝╚═╝  ╚═╝╚══════╝
                < View in "Raw" on GitHub >
-}
--
-- Step (3) of compiling.
--
-- Parser module takes the list of (Token, location) tuples generated by Lexer.hs and Relex.hs,
-- and generates an AST
--
{- NOTE TO READER: This may differ slightly from implementation, check the FIRST and FOLLOW functions at the bottom!
+###########+######+###################################################+##############################+
|           | NULL |                       FIRST                       |            FOLLOW            |
+###########+######+###################################################+##############################+
| Prog      | N    | imort                                             |                              |
+-----------+------+---------------------------------------------------+------------------------------+
| MainClass | N    | class                                             | class, EOF                   |
+-----------+------+---------------------------------------------------+------------------------------+
| MD        | Y    | public                                            | }                            |
+-----------+------+---------------------------------------------------+------------------------------+
| CD        | Y    | class                                             | EOF                          |
+-----------+------+---------------------------------------------------+------------------------------+
| VD        | Y    | FIRST(type)                                       | public, FIRST(SL), }, return |
+-----------+------+---------------------------------------------------+------------------------------+
+===========+======+===================================================+==============================+
+-----------+------+---------------------------------------------------+------------------------------+
| SL        | Y    | FIRST(Stm)                                        |  return, }                   |
+-----------+------+---------------------------------------------------+------------------------------+
| Stm       | N    | if, while, {, MeggySetPix, MeggyToneStart,        | FOLLOW(SL), FIRST(Stm)       |
+-----------+------+---------------------------------------------------+------------------------------+
|           |      | MeggyDelay, MeggyAuxleds, FIRST(J)                |                              |
+-----------+------+---------------------------------------------------+------------------------------+
| A         | Y    | else                                              | FOLLOW(Stm)                  |
+-----------+------+---------------------------------------------------+------------------------------+
+===========+======+===================================================+==============================+
+-----------+------+---------------------------------------------------+------------------------------+
| E         | N    | FIRST(J)                                          | comma, dot, ), semicolon, ]  |
+-----------+------+---------------------------------------------------+------------------------------+
| E'        | Y    | &&                                                | FOLLOW(E)                    |
+-----------+------+---------------------------------------------------+------------------------------+
| F         | N    | FIRST(J)                                          | &&, FOLLOW(E)                |
+-----------+------+---------------------------------------------------+------------------------------+
| F'        | Y    | ==                                                | FOLLOW(F)                    |
+-----------+------+---------------------------------------------------+------------------------------+
| G         | N    | FIRST(J)                                          | ==, FOLLOW(F)                |
+-----------+------+---------------------------------------------------+------------------------------+
| G'        | Y    | <                                                 | FOLLOW(G)                    |
+-----------+------+---------------------------------------------------+------------------------------+
| H         | N    | FIRST(J)                                          | <, FOLLOW(G)                 |
+-----------+------+---------------------------------------------------+------------------------------+
| H'        | Y    | +,-                                               | FOLLOW(H)                    |
+-----------+------+---------------------------------------------------+------------------------------+
| I         | N    | FIRST(J)                                          |  +, -,FOLLOW(H)              |
+-----------+------+---------------------------------------------------+------------------------------+
| I'        | Y    | *                                                 | FOLLOW(I)                    |
+-----------+------+---------------------------------------------------+------------------------------+
| J         | N    | new, bytecast, FIRST(K)                           | * FOLLOW(K)                  |
+-----------+------+---------------------------------------------------+------------------------------+
| P         | N    | TokenID, MeggyColor, int                          | FOLLOW(J)                    |
+-----------+------+---------------------------------------------------+------------------------------+
| K         | N    | !, -, FIRST(L)                                    | FOLLOW(J)                    |
+-----------+------+---------------------------------------------------+------------------------------+
| L         | N    | ColorVal, ButtonVal, ToneVal, MeggyGetPix, this   | FOLLOW(K)                    |
+-----------+------+---------------------------------------------------+------------------------------+
|           |      | MeggyCheckButt, dot, (, int, true, false, TokenID |                              |
+-----------+------+---------------------------------------------------+------------------------------+
| Id        | N    | TokenID                                           | {, (, comma, ), [, =         |
+-----------+------+---------------------------------------------------+------------------------------+
+===========+======+===================================================+==============================+
+-----------+------+---------------------------------------------------+------------------------------+
| Type      | N    | boolean, byte, int, MeggyColor, MeggyTone         | FIRST(ID)                    |
+-----------+------+---------------------------------------------------+------------------------------+
|           |      | MeggyButton, MeggyColor[ ], int[ ], void          |                              |
+-----------+------+---------------------------------------------------+------------------------------+
|           |      |                                                   |                              |
+-----------+------+---------------------------------------------------+------------------------------+
| Invoke    | N    | dot                                               | FOLLOW(L)                    |
+-----------+------+---------------------------------------------------+------------------------------+
| Param     | Y    | FIRST(E)                                          | )                            |
+-----------+------+---------------------------------------------------+------------------------------+
| X         | N    | FIRST(E)                                          | comma )                      |
+-----------+------+---------------------------------------------------+------------------------------+
| Y         | Y    | comma                                             | )                            |
+-----------+------+---------------------------------------------------+------------------------------+
+===========+======+===================================================+==============================+
+-----------+------+---------------------------------------------------+------------------------------+
| ParamDecl | Y    | FIRST(Type)                                       | )                            |
+-----------+------+---------------------------------------------------+------------------------------+
| U         | N    | FIRST(Type)                                       | comma )                      |
+-----------+------+---------------------------------------------------+------------------------------+
| V         | Y    | comma                                             | )                            |
+-----------+------+---------------------------------------------------+------------------------------+
|           |      |                                                   |                              |
+-----------+------+---------------------------------------------------+------------------------------+
| Z         | Y    | return                                            | }                            |
+-----------+------+---------------------------------------------------+------------------------------+
-}

module Parser where

import Lexer
import Util

-- This is the top level function main will call to build the ast.
-- Return the root node of the AST.
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
-- Production rule:
-- Prog -> "import" "meggy" "." "Meggy" ";" MC CD $ 
parseProg :: [(Token, (Int,Int))] -> AST
parseProg ts =
    let
        ts1                  = match ts TokenMeggyImport
        (main_class, ts2)    = parseMC ts1
        other_classes        = parseCD ts2
    in
        (Prog main_class other_classes)

-- MC production (Main Class)
-- Production rule:
-- MC -> "class" ID "{" "public" "static" "void" "main" "(" "String" "[]" ID "")" "{" SL "}" "}"
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

-- CD production (Class Declaration)
-- Production rule:
-- "class" ID "{" VD MD "}" CD | Epsilon
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


-- VD production (Variable Declaration)
-- Production rule:
-- VD -> Type ID ";" VD | Epsilon
parseVD :: [(Token, (Int,Int))] -> (AST, [(Token, (Int,Int))])
parseVD ts1 = 
    let
        (vars, ts2) = parseVD' ts1
    in
        (VarDecl vars, ts2)

parseVD' :: [(Token, (Int,Int))] -> ([AST], [(Token, (Int,Int))])
parseVD' ts
    | VoidType == vtype = error ("ERR: (Parser - ParseVD') Cannot have a void type vairable\n")
    | "" == vname = ([], ts1)
    | otherwise =
        let
            var1 = Variable vtype vname
            ts2  = match ts1 TokenSemiColon
            (var2, ts3) = parseVD' ts2
        in
            (var1:var2, ts3)
    where
        ((vname, vtype), ts1) = parseU ts
        
-- MD Production (Method Declaration)
-- Production rule:
-- MD -> "public" Type ID "(" ParamDecl ")" "{" VD SL Z "}" MD | Epsilon
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

-- Z production (Return expression)
-- Production rule:
-- Z -> "return" E ";" | Epsilon
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

-- ParamDecl (Parameter declarations in method signatures)
-- Production rule:
-- ParamDecl -> U V | Epsilon
-- U         -> Type ID
-- V         -> "," U V | Epsilon
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

-- Helper production for ParamDecl
parseU :: [(Token, (Int,Int))] -> ((String, Type), [(Token, (Int,Int))])
parseU all@((TokenPublic, (row, col)):rest) = (("", Error), all)
parseU all@((TokenReturn, (row, col)):rest) = (("", Error), all)
parseU ((typeTok, (row,col)):((TokenID name), (r1,c1)):rest) =
    if first_Type typeTok then
        ((name, (grab_Type typeTok)), rest)
    else
        error ("ERR: (Parser - ParseU) on token " ++ show typeTok ++ " at [" ++ show row ++ ", " ++ show col ++ "]\n")

parseU ts@((t, (row,col)):rest) =
    if follow_VD t then
        (("", Error), ts)
    else
        error ("ERR: (Parser - ParseU) on token " ++ show t ++ " at [" ++ show row ++ ", " ++ show col ++ "]\n")

-- Helper production for ParamDecl
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

-- SL Production (Statement List)
-- Production rule:
-- SL -> Stm SL | Epsilon
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

-- Statement Production (Stm)
-- Production rule:
-- Stm -> "if" "(" E ")" Stm A
--     -> "while" "(" E ")" Stm
--     -> "{" SL "}"
--     -> "Meggy.setAuxLEDs" "(" E ")" ";"
--     -> "Meggy.delay" "(" E ")" ";"
--     -> "Meggy.setPixel" "(" E "," E "," E ")" ";"
--     -> "Meggy.toneStart" "(" E ","" E ")" ";"
--     ->  E Invoke ";"
--     ->  ID "=" E ";"
--     ->  ID "[" E "]" "=" E ";"
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

parseStm ((TokenMeggySetAux, (row, col)):rest) = 
    let
        ts1 = match rest TokenLeftParen
        (exp, ts2) = parseE ts1
        ts3 = match ts2 TokenRightParen
        ts4 = match ts3 TokenSemiColon
    in
        ((SetAuxLEDs exp), ts4)

parseStm ts@((TokenNew, (row,col)):(TokenID id, (r1,c1)):rest) = 
    let
        (exp, ts1) = parseE ts
        ts2 = match ts1 TokenSemiColon
    in
        (exp, ts2)

parseStm ((TokenThis, (row,col)):rest) = 
    let
        ts1 = match rest TokenDot
        (invocation, ts2) = parseInvoke ts1 (Instance "this")
        ts3 = match ts2 TokenSemiColon
    in
        (invocation, ts3)

-- Id = E PostE ; 
parseStm ((TokenID name, (r1,c1)):(TokenAssign, (r2,c2)):rest) =
    let
        (exp, ts1) = parseE rest
        ts2 = match ts1 TokenSemiColon
    in
        (Assignment name exp, ts2)

-- Id [ E ] = E PostE ;
parseStm ((TokenID name, (r1,c1)):(TokenLeftBracket, (r2,c2)):rest) =
    let
        (index, ts1) = parseE rest
        ts2 = match ts1 TokenRightBracket
        ts3 = match ts2 TokenAssign
        (exp, ts4) = parseE ts3
        ts5 = match ts4 TokenSemiColon
    in
        (ArrayAssignment name index exp, ts5)

parseStm all@((TokenID name, (r1,c1)):(TokenDot, (r2,c2)):rest) =
    let
        (ast, ts1) = parseE all
        ts2        = match ts1 TokenSemiColon
    in
        (ast, ts2)

parseStm ((t, (row,col)):rest) = 
    error ("ERR: (Parser - ParseStm) Invalid instance on token string " ++ show t ++ "... starting at [" ++ show row ++ ", " ++ show col ++ "]\n")

-- A Production (The optional else body)
-- Production rule:
-- A -> "else" Stm | Epsilon
parseA :: [(Token, (Int,Int))] -> (AST, [(Token, (Int,Int))])
parseA ((TokenElse, (row,col)):rest) = 
    let
        (stmt, ts1)    = parseStm rest
    in
        (stmt, ts1)

--Check the follow set, if it is in the follow of A, produce Epsilon
parseA all@((t, (row,col)):rest) = 
    if follow_A t then
        (Epsilon, all) 
    else 
        error("ERR: (Parser - ParseA) Invalid else body on token " ++ show t ++ " at [" ++ show row ++ ", " ++ show col ++ "]\n")

-- E Production (Expression)
-- Production rule:
-- E  ->  FE'
-- E' -> "&&" FE' | Epsilon
-- F  ->  GF'
-- F' -> "==" GF' | Epsilon
-- G  ->  HG' 
-- G' -> "<" HG'  | Epsilon
-- H  ->  IH'
-- H' -> "+" IH' | "-" IH" | Epsilon
-- I  ->  JI'
-- I' -> "*" JI'  | Epsilon
-- J  -> "new" P | ByteCast J | K
--             P -> ID "(" ")" PostE | "Meggy.Color" "[" "]" PostE | int "[" "]" PostE 
-- K  -> "!" K | "-" J | L
-- L  ->  Meggy.Color._ | Meggy.Button._ | Meggy.Tone._ | 
--    -> "Meggy.getPixel" "(" E "," E ")" | "Meggy.checkButton" "(" E ")"
--    -> "(" E ")" | <Integer> | <boolean> | "this" | ID | E "[" E "]"
-- ID -> <Identifier String> 
-- PostE -> This production is a bit of an oddball but necessary for nesting of function
--          calls and array nestings. 
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

--J
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

--P (Disambiguation of "new ..." statements from J)
parseP :: [(Token, (Int,Int))] -> (AST, [(Token, (Int,Int))])
parseP ((TokenID id, (r1,c1)):(TokenLeftParen, (r2,c2)):(TokenRightParen, (r3,c3)):rest) = 
    parsePostE rest (Instance id)        

parseP ((TokenInt, (r1,c1)):(TokenLeftBracket, (r2,c2)):rest) =
    let
        (capacity, ts1) = parseE rest
        ts2@((someToken, (x,y)):r) = match ts1 TokenRightBracket
    in
        -- If we find second array access it may be a 2d array or bad syntax for accessing the element of a newly instantiated array.
        if someToken == TokenLeftBracket then 
            error ("Parsing Error in parsePostE on token " ++ show someToken ++ " at [" ++ show x ++ ", " ++ show y ++ "]\n")
        else
            parsePostE ts2 (IntArrayInstance capacity)

parseP ((TokenMeggyColorType, (r1,c1)):(TokenLeftBracket, (r2,c2)):rest) =
    let
        (capacity, ts1) = parseE rest
        ts2@((someToken, (x,y)):r) = match ts1 TokenRightBracket
    in
        -- If we find second array access it may be a 2d array or bad syntax for accessing the element of a newly instantiated array.
        if someToken == TokenLeftBracket then 
            error ("Parsing Error in parsePostE on token " ++ show someToken ++ " at [" ++ show x ++ ", " ++ show y ++ "]\n")
        else
            parsePostE ts2 (ColorArrayInstance capacity)

parseP ts = error("Error: parseP "++ (show $ head ts))

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

--L (Values of expressions)
parseL  :: [(Token, (Int,Int))] -> (AST, [(Token, (Int,Int))])

-- Basic lvalues
parseL ((TokenNum x, (row,col)):rest)                                     = (IntLiteral x, rest)
parseL ((TokenMeggyColor, (row,col)) :(TokenColorValue x, (r1,c1)):rest)  = (ColorLiteral x, rest)
parseL ((TokenMeggyButton, (row,col)):(TokenButtonValue x, (r1,c1)):rest) = (ButtonLiteral x, rest)
parseL ((TokenMeggyTone, (row,col))  :(TokenToneValue x, (r1,c1)):rest)   = (ToneLiteral x, rest)
parseL ((TokenTrue, (row,col)):rest)   = (Boolean True, rest)
parseL ((TokenFalse, (row,col)):rest)  = (Boolean False, rest)
parseL ((TokenID id, (row,col)):rest)  = parsePostE rest (Identifier id)

-- Expression values
parseL ((TokenThis, (row,col)):rest) = parsePostE rest (Instance "this")
parseL ((TokenLeftParen, (row,col)):rest) = 
    let
        (exp, ts1)       = parseE rest
        ts2              = match ts1 TokenRightParen
    in
        parsePostE ts2 (ParenExp exp)
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
    error("Parsing Error in parseL on token " ++ show t ++ " at [" ++ show row ++ ", " ++ show col ++ "]\n")

-- PostE (Ability for an expression to be a nesting of method calls and array accesses and stuff)
parsePostE :: [(Token, (Int,Int))] -> AST -> (AST, [(Token, (Int,Int))])
parsePostE ((TokenDotLength, (row,col)):rest) receiver = (ArrayLength receiver, rest)
parsePostE ((TokenDot, (row,col)):rest) receiver = 
    let 
        (new_receiver, ts1) = parseInvoke rest receiver
    in 
        parsePostE ts1 new_receiver 

parsePostE ((TokenLeftBracket, (row,col)):rest) receiver = 
    let
        (index, ts1) = parseE rest
        ts2 = match ts1 TokenRightBracket
    in  
        (ArrayAccess receiver index, ts2)

parsePostE all@((t, (row,col)):rest) ast =
        (ast, all)


-- Invoke Production (Method Invocation Grammar)
-- Production rule:
-- Invoke -> ID "(" Param ")" 
parseInvoke :: [(Token, (Int,Int))] -> AST -> (AST, [(Token, (Int,Int))])
parseInvoke ((TokenID mname, (row,col)):rest) receiver = 
    let
        ts1 = match rest TokenLeftParen
        (params, ts2) = parseParam ts1
        -- TokenRightParen matched by parseParam
    in
        (Invoke receiver params mname, ts2)

-- Param Produciton (Arguments to a method invocation)
-- Production rule:
-- Param -> X Y | Epsilon
--   X   -> E
--   Y   -> "," X Y | Epsilon
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

-- Given the list of tokens check if the next token matches the token passed in.
-- If they match, consume the token and return the list without that token.
-- If they do not match, report and error.
match :: [(Token, (Int,Int))] -> Token -> [(Token, (Int,Int))]
match ((given, (row,col)):rest) expected =
    if given == expected then 
        rest
    else error (   "ERR: (Parser - match)" 
                ++ " ["++(show row)++", " ++ (show col) ++ "]"
                ++ "\n\t -> Expected " ++ (show expected) ++ " but found " ++ (show given) ++ "\n")

-- FIRST and FOLLOW sets from predictive parser (See top of this file)
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
        TokenReturn         -> True
        _                   -> first_Expression t


-- tokens in FIRST(E) are tokens in ( TokenNew U TokenByteCast U FIRST(K) U FIRST(L) ) 
first_Expression :: Token -> Bool
first_Expression t = 
    let
        inK = first_K t
    in 
        if inK then
            inK
        else
            case t of 
                TokenNew       -> True
                TokenByteCast  -> True
                _ -> False

-- tokens in FIRST(K) are ( TokenNot U TokenSub U FIRST(L) ) 
first_K :: Token -> Bool
first_K t =
    let
        inL = first_L t
    in
        if inL then
            inL
        else
            case t of 
                TokenNot   -> True
                TokenSub   -> True
                _ -> False

-- tokens in FIRST(L) are:
first_L :: Token -> Bool
first_L t =
    case t of 
        TokenMeggyColor  -> True
        TokenMeggyButton -> True
        TokenMeggyTone   -> True
        TokenMeggyGetPix -> True
        TokenMeggyCheckButton -> True
        TokenLeftParen        -> True
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
        TokenID _            -> True -- A class name can be a type
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
        TokenID class_name   -> ClassType class_name --Violates the pattern we had used for Types, but I think we need the name of the class here?
        _ -> error ("ERR: (Parser - grab_Type) Did not expect " ++ show t ++ "as a type \n")

follow_E :: Token -> Bool
follow_E  t = 
    case t of 
        TokenComma -> True
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

follow_A :: Token -> Bool
follow_A TokenRightCurly = True
follow_A t = first_Stm t