{-              < View in "Raw" on GitHub >

██╗     ███████╗██╗  ██╗███████╗██████╗    ██╗  ██╗███████╗
██║     ██╔════╝╚██╗██╔╝██╔════╝██╔══██╗   ██║  ██║██╔════╝
██║     █████╗   ╚███╔╝ █████╗  ██████╔╝   ███████║███████╗
██║     ██╔══╝   ██╔██╗ ██╔══╝  ██╔══██╗   ██╔══██║╚════██║
███████╗███████╗██╔╝ ██╗███████╗██║  ██║██╗██║  ██║███████║
╚══════╝╚══════╝╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝╚═╝╚═╝  ╚═╝╚══════╝
                < View in "Raw" on GitHub >
-}
--
-- Step (1) of compiling.
--
-- Lexer module to produce all tokens from input file.
--
--  **This code is a modified version of the base code provided to us by the authors
-- of Dr. Strout and Patrick Hickey**
--

module Lexer where

import Data.Char -- needed for isChar, isAlpha, isSpace, isDigit

-- Keep having driveTable produce tokens until the input string
-- has been completely consumed. This is the entry point
lexer :: String -> [(Token, (Int,Int))] -- Input goes to -> list of Token, location tuples
lexer input = let tokens = (lexer' (input ++ " ") (1, 1)) in (tokens++[(TokenEOF, (-1,-1))]) -- Put at least one white space character at end of file so that
                                                                                             -- [other] transitions in DFA work. And start at (1,1) for location

lexer' :: String -> (Int,Int) -> [(Token, (Int,Int))]
lexer' [] (_,_)           = [] -- finished
lexer' input (row,col)    =
    let -- start in state 0 with an empty token string
        ((new_row,new_col), pair@(tok, loc),remaining) = driveTable (row,col) 0 "" input
    in
        if new_col == -666 then
            (TokenMeggyButton, (row,col)) : pair :  lexer' remaining (new_row,col)
        else
            case tok of
                WhiteSpace   -> lexer' remaining (new_row,new_col)
                TokenNewLine -> lexer' remaining (new_row,new_col)
                TokenComment -> lexer' remaining (new_row,new_col)
                TokenUnknown s  -> error ("ERR: (Lexer) -> Illegal Identifier \"" ++ s ++ "...\" at [" ++ (show new_row) ++ ", " ++ (show new_col) ++ "]")
                _ -> pair : (lexer' remaining (new_row,new_col) )

-- Table driven analysis.
-- From given state will continue to add to the first string
-- and consume characters from the second string until a token is found.
-- Evaluates to (token, location) tuple and remaining string.
driveTable :: (Int,Int) -> Int -> String -> String -> ((Int,Int),(Token, (Int,Int)),String)
driveTable (row,col) currState currTokStr []       = ((row,col),(UnexpectedEOF currTokStr, (row, (col - length currTokStr))), "")
driveTable (row,col) 0 "" ('M':'e':'g':'g':'y':'.':'B':'u':'t':'t':'o':'n':'.':'A':rest) =
    ((row+14,-666),((TokenButtonValue "A"),(row,col)),rest)
driveTable (row,col) 0 "" ('M':'e':'g':'g':'y':'.':'B':'u':'t':'t':'o':'n':'.':'B':rest) =
    ((row+14,-666),((TokenButtonValue "B"),(row,col)),rest)
driveTable (row,col) 0 "" ('M':'e':'g':'g':'y':'.':'B':'u':'t':'t':'o':'n':'.':'U':'p':rest) =
    ((row+15,-666),((TokenButtonValue "Up"),(row,col)),rest)
driveTable (row,col) 0 "" ('M':'e':'g':'g':'y':'.':'B':'u':'t':'t':'o':'n':'.':'D':'o':'w':'n':rest) =
    ((row+17,-666),((TokenButtonValue "Down"),(row,col)),rest)
driveTable (row,col) 0 "" ('M':'e':'g':'g':'y':'.':'B':'u':'t':'t':'o':'n':'.':'L':'e':'f':'t':rest) =
    ((row+17,-666),((TokenButtonValue "Left"),(row,col)),rest)
driveTable (row,col) 0 "" ('M':'e':'g':'g':'y':'.':'B':'u':'t':'t':'o':'n':'.':'R':'i':'g':'h':'t':rest) =
    ((row+18,-666),((TokenButtonValue "Right"),(row,col)),rest)
driveTable (row,col) currState currTokStr (c:rest) =
    let 
        ((new_row,new_col),next,consume) = nextState (row,col) currState c
        (nextTokStr,remaining)  = nextStrings currTokStr c rest consume 
        (done,tok)              = final (new_row,new_col) next nextTokStr
    in
        if done then 
            ((new_row,new_col),tok,remaining)
        else driveTable (new_row,new_col) next nextTokStr remaining

-- Determine the collected token string and what is left given
-- the current token string, the current character being processed,
-- the remaining input string, and whether to consume the character.
nextStrings :: String -> Char -> String -> Bool -> (String,String)
nextStrings tokStr c remaining consume 
    | consume      = (tokStr ++ [c], remaining)
    | not consume  = (tokStr       , c:remaining)

-- **************************** Specific to one lexer
-- DFA for implementation.
--
--   Identifiers and keywords
--      0 -- isLetter --> 1         // have seen a letter
--      1 -- isLetter --> 1
--      1 -- [other]  --> 2         // (final) look in ID dictionary for token
--
--   Numbers
--      0  --> isDigit --> 10
--      10 --> isDigit --> 10
--      10 --> [other] --> 11       // (final) number
--
--      0 --> isSpace --> 99        // (final) WhiteSpace, won't be returned.


-- Token datatype.
-- TokenNUM and TokenID require an Int and String respectively.
-- Deriving Show so these tokens can be printed. 
-- Deriving Eq enables these tokens to be compared with == operator.
data Token
    --Handle imort statement keywords
    = TokenImport      -- import
    | TokenLittleMeggy  -- meggy
    | TokenBigMeggy     -- Meggy

    --Syntax tokens
    | TokenDot          -- .
    | TokenLeftCurly    -- {
    | TokenRightCurly   -- }
    | TokenLeftBracket  -- [
    | TokenRightBracket -- ]
    | TokenLeftParen    -- (
    | TokenRightParen   -- )
    | TokenComma        -- ,
    | TokenSemiColon    -- ;

    --Comments
    | TokenComment           -- Block and single line

    --other java keywords
    | TokenClass        -- class
    | TokenPublic       -- public
    | TokenStatic       -- static
    | TokenVoid         -- void
    | TokenMain         -- main
    | TokenIf           -- if
    | TokenElse         -- else
    | TokenWhile        -- while

    --types
    | TokenString       -- String
    | TokenByte         -- byte

    --Meggy types and keywords
    | TokenColorValue Int     -- Meggy.Color.<X> | X is the int color value
    | TokenButtonValue String -- Meggy.Button.<X> | X is the button value string
    | TokenColor        -- Color 
    | TokenButton       -- Button
    
    --Meggy methods
    | TokenSetPixel     -- Meggy.setPixel T
    | TokenGetPixel     -- Meggy.getPixel 
    | TokenCheckButton  -- Meggy.checkButton 
    | TokenDelay        -- Meggy.delay 

    --Identifiers and number literals
    | TokenID String    -- variable1 (any valid java identifier)
    | TokenNum Int      -- 12        (any sequence of only digits)

    --Operators
    | TokenSub          -- -
    | TokenAdd          -- +
    | TokenMul          -- *
    | TokenAnd          -- &&
    | TokenEquality     -- ==
    | TokenNot          -- !

    --Booleans
    | TokenTrue         -- true
    | TokenFalse        -- false

    --PA4
    | TokenReturn        -- return
    | TokenToneValue Int -- Meggy.Tone.<T> where T is the tone number from the reference compiler
    | TokenInt           -- int
    | TokenBoolean       -- boolean (as in a return type)
    | TokenToneStart     -- Meggy.toneStart
    | TokenLessThan      -- <
    | TokenThis          -- this
    | TokenNew           -- new
    | TokenTone

    {-
      Relex tokens:
        Relex is a second lexer pass to simplify the token strings. This helps alleviate
        issues with LL(1) parsing. Instead of dealing with the ambiguity of seeing the "Meggy"
        token, we can be confident that we will get a TokenMeggySetPix instead of 
        "Meggy" "." "setPixel". Quite helpful. See ReLex.hs for more documentation.
        Another example is the first line of every meggy java program:
            [TokenImport, TokenMeggy, TokenDot, TokenBigMeggy, TokenSemicolon] => [TokenMeggyImport]
        or:
            [TokenInt, TokenLeftBracket, TokenRightBracket] => [TokenIntArrayType]
    -}
    | TokenMeggySetPix
    | TokenMeggyDelay
    | TokenMeggyGetPix
    | TokenMeggyToneStart
    | TokenMeggyCheckButton
    | TokenMeggyTone
    | TokenMeggyColor
    | TokenMeggyButton
    | TokenMeggyImport
    | TokenMeggyColorType
    | TokenMeggyButtonType
    | TokenMeggyToneType
    | TokenByteCast

    --PA5 Stuff
    | TokenIntArrayType
    | TokenColorArrayType
    | TokenSetAuxLEDs
    | TokenMeggySetAux
    | TokenLength
    | TokenDotLength
    | TokenAssign

    --misc
    | WhiteSpace
    | TokenNewLine
    | UnexpectedEOF String -- usually for unclosed block comments
    | TokenEOF
    | TokenUnknown String -- usually for illegal identifiers
    deriving (Show,Eq)

-- Lookup keyword tokens.
lookupKW :: (Int,Int) -> String -> (Token, (Int,Int))
lookupKW (row,col) keyword
    -- Literal keywords
    | keyword  == "DARK"        = (TokenColorValue 0, (row,(col - (length keyword))))
    | keyword  == "RED"         = (TokenColorValue 1, (row,(col - (length keyword))))
    | keyword  == "ORANGE"      = (TokenColorValue 2, (row,(col - (length keyword))))
    | keyword  == "YELLOW"      = (TokenColorValue 3, (row,(col - (length keyword))))
    | keyword  == "GREEN"       = (TokenColorValue 4, (row,(col - (length keyword))))
    | keyword  == "BLUE"        = (TokenColorValue 5, (row,(col - (length keyword))))
    | keyword  == "VIOLET"      = (TokenColorValue 6, (row,(col - (length keyword))))
    | keyword  == "WHITE"       = (TokenColorValue 7, (row,(col - (length keyword))))
    -- | keyword  == "Up"          = (TokenButtonValue "Up", (row,(col - (length keyword))))
    -- | keyword  == "Down"        = (TokenButtonValue "Down", (row,(col - (length keyword))))
    -- | keyword  == "Left"        = (TokenButtonValue "Left", (row,(col - (length keyword))))
    -- | keyword  == "Right"       = (TokenButtonValue "Right", (row,(col - (length keyword))))
    -- | keyword  == "B"           = (TokenButtonValue "B", (row,(col - (length keyword))))
    -- | keyword  == "A"           = (TokenButtonValue "A", (row,(col - (length keyword))))
    | keyword  == "C3"          = (TokenToneValue 61157, (row,(col - (length keyword))))
    | keyword  == "Cs3"         = (TokenToneValue 57724, (row,(col - (length keyword))))
    | keyword  == "D3"          = (TokenToneValue 54485, (row,(col - (length keyword))))
    | keyword  == "Ds3"         = (TokenToneValue 51427, (row,(col - (length keyword))))
    | keyword  == "E3"          = (TokenToneValue 48541, (row,(col - (length keyword))))
    | keyword  == "F3"          = (TokenToneValue 45816, (row,(col - (length keyword))))
    | keyword  == "Fs3"         = (TokenToneValue 43243, (row,(col - (length keyword))))
    | keyword  == "G3"          = (TokenToneValue 40816, (row,(col - (length keyword))))
    | keyword  == "Gs3"         = (TokenToneValue 38526, (row,(col - (length keyword))))
    | keyword  == "A3"          = (TokenToneValue 36363, (row,(col - (length keyword))))
    | keyword  == "As3"         = (TokenToneValue 34323, (row,(col - (length keyword))))
    | keyword  == "B3"          = (TokenToneValue 32397, (row,(col - (length keyword))))
    | keyword  == "true"        = (TokenTrue, (row,(col - (length keyword))))
    | keyword  == "false"       = (TokenFalse, (row,(col - (length keyword))))

    -- Meggy library keywords
    | keyword  == "checkButton" = (TokenCheckButton, (row,(col - (length keyword))))
    | keyword  == "getPixel"    = (TokenGetPixel,    (row,(col - (length keyword))))
    | keyword  == "setPixel"    = (TokenSetPixel,    (row,(col - (length keyword))))
    | keyword  == "toneStart"   = (TokenToneStart,   (row,(col - (length keyword))))
    | keyword  == "delay"       = (TokenDelay,       (row,(col - (length keyword))))
    | keyword  == "setAuxLEDs"  = (TokenSetAuxLEDs,  (row, (col - (length keyword))))

    -- Java/General keywords
    | keyword  == "import"      = (TokenImport, (row,(col - (length keyword))))
    | keyword  == "meggy"       = (TokenLittleMeggy, (row,(col - (length keyword))))
    | keyword  == "Meggy"       = (TokenBigMeggy, (row,(col - (length keyword))))
    | keyword  == "public"      = (TokenPublic, (row,(col - (length keyword))))
    | keyword  == "static"      = (TokenStatic, (row,(col - (length keyword))))
    | keyword  == "main"        = (TokenMain, (row,(col - (length keyword))))
    | keyword  == "length"      = (TokenLength, (row,(col - (length keyword))))
    
    -- Control flow
    | keyword  == "if"          = (TokenIf, (row,(col - (length keyword))))
    | keyword  == "else"        = (TokenElse, (row,(col - (length keyword))))
    | keyword  == "while"       = (TokenWhile, (row,(col - (length keyword))))
    | keyword  == "return"      = (TokenReturn, (row,(col - (length keyword))))

    -- Class/Instance keywords
    | keyword  == "class"       = (TokenClass, (row,(col - (length keyword))))
    | keyword  == "this"        = (TokenThis, (row,(col - (length keyword))))
    | keyword  == "new"         = (TokenNew, (row,(col - (length keyword))))

    -- Type keywords
    | keyword  == "boolean"     = (TokenBoolean, (row,(col - (length keyword))))
    | keyword  == "byte"        = (TokenByte, (row,(col - (length keyword))))
    | keyword  == "int"         = (TokenInt, (row,(col - (length keyword))))
    | keyword  == "void"        = (TokenVoid, (row,(col - (length keyword))))
    | keyword  == "Color"       = (TokenColor, (row,(col - (length keyword))))
    | keyword  == "Button"      = (TokenButton, (row,(col - (length keyword))))
    | keyword  == "Tone"        = (TokenTone, (row,(col - (length keyword))))
    | keyword  == "String"      = (TokenString, (row,(col - (length keyword))))
    | otherwise                 = (TokenID keyword, (row,(col - (length keyword))))

-- Indicate which states are final states and the
-- Token associated with them.
final :: (Int,Int) -> Int -> String -> (Bool,(Token, (Int,Int)))
final (row,col)  3  curTokStr   = (True, lookupKW (row,col) curTokStr)

final (row,col)  4  curTokStr   = (True, (WhiteSpace, (row,col)))
final (row,col)  5  curTokStr   = (True, (TokenNum (read curTokStr), (row,col)))

final (row,col)  6   curTokStr  = (True, (TokenNot, (row,col)))
final (row,col)  7   curTokStr  = (True, (TokenDot, (row,col)))
final (row,col)  8   curTokStr  = (True, (TokenSemiColon, (row,col)))
final (row,col)  9   curTokStr  = (True, (TokenLeftParen, (row,col)))
final (row,col)  10  curTokStr  = (True, (TokenRightParen, (row,col)))
final (row,col)  11  curTokStr  = (True, (TokenLeftBracket, (row,col)))
final (row,col)  12  curTokStr  = (True, (TokenRightBracket, (row,col)))
final (row,col)  13  curTokStr  = (True, (TokenLeftCurly, (row,col)))
final (row,col)  14  curTokStr  = (True, (TokenRightCurly, (row,col)))
final (row,col)  15  curTokStr  = (True, (TokenComma, (row,col)))
final (row,col)  16  curTokStr  = (True, (TokenNewLine, (row,col)))
final (row,col)  21  curTokStr  = (True, (TokenMul, (row,col)))
final (row,col)  22  curTokStr  = (True, (TokenAnd, (row,col)))
final (row,col)  23  curTokStr  = (True, (TokenEquality, (row,col)))
final (row,col)  24  curTokStr  = (True, (TokenSub, (row,col)))
final (row,col)  25  curTokStr  = (True, (TokenAdd, (row,col)))
final (row,col)  26  curTokStr  = (True, (TokenMul, (row,col)))
final (row,col)  27  curTokStr  = (True, (TokenLessThan, (row,col)))
final (row,col)  28  curTokStr  = (True, (TokenNewLine, (row,col)))
final (row,col)  29  curTokStr  = (True, (TokenAssign, (row,col)))

final (row,col)  50  curTokStr  = (True, (TokenComment, (row,col)))

final (row,col) 999 curTokStr  = (True, (TokenUnknown curTokStr, (row,col)))
final (row,col) _   curTokStr  = (False,(TokenUnknown curTokStr, (row,col)))

-- Encoding the table as the nextState function.
-- Given the current state and the next character,
-- evaluates to next state and whether the character
-- should be consumed or not.
nextState :: (Int,Int) -> Int -> Char -> ((Int,Int),Int,Bool)

nextState (row,col) 0 '!'     = ( (row,col + 1),  6,  True)
nextState (row,col) 0 '.'     = ( (row,col + 1),  7,  True)
nextState (row,col) 0 ';'     = ( (row,col + 1),  8,  True)
nextState (row,col) 0 '('     = ( (row,col + 1),  9,  True)
nextState (row,col) 0 ')'     = ( (row,col + 1),  10, True)
nextState (row,col) 0 '['     = ( (row,col + 1),  11, True)
nextState (row,col) 0 ']'     = ( (row,col + 1),  12, True)
nextState (row,col) 0 '{'     = ( (row,col + 1),  13, True)
nextState (row,col) 0 '}'     = ( (row,col + 1),  14, True)
nextState (row,col) 0 ','     = ( (row,col + 1),  15, True)
nextState (row,col) 0 '-'     = ( (row,col + 1),  24, True)
nextState (row,col) 0 '+'     = ( (row,col + 1),  25, True)
nextState (row,col) 0 '*'     = ( (row,col + 1),  26, True)
nextState (row,col) 0 '<'     = ( (row,col + 1),  27, True)
nextState (row,col) 0 '\n'    = ( (row + 1,  0),  28, True)
nextState (row,col) 0 '&'     = ( (row,col + 1),  32, True)
nextState (row,col) 0 '='     = ( (row,col + 1),  33, True)

nextState (row,col) 0 c
    | isDigit c  = ( (row,col + 1),  1, True) 
    | isLetter c = ( (row,col + 1),  2, True)
    | c == '$'   = ( (row,col + 1),  2, True)
    | c == '_'   = ( (row,col + 1),  2, True)
    | c == '/'   = ( (row,col + 1),  30,True)
    | c == ' '   = ( (row,col + 1),  4, True)
    | c == '\t'  = ( (row,col + 4),  4, True) -- Assumes tabs are four spaces, mileage may vary depending on system.
                                              -- Why are you even using tabs? This is yet another example of why tabs are
                                              -- clearly inferior, you have to deal with tabs edge cases, does that seem like
                                              -- a fun thing to worry about? Use spaces. 4 space tab replacement. Save a life.  ;-}
    | isSpace c  = ( (row,col + 1),  4, True)
    | otherwise  = ( (row,col + 1),  2, True)

nextState (row,col) 1 c -- for digits
    | isDigit c  = ( (row,col + 1),  1, True)
    | isLetter c || c == '_' || c == '$' = ( (row,col), 999, False)
    | otherwise  = ( (row,col),  5, False)

nextState (row,col) 2 c -- for ID [letters, $, _ and numbers]
    | isLetter c = ( (row,col + 1),  2, True)
    | isDigit c  = ( (row,col + 1),  2, True)
    | c == '_'   = ( (row,col + 1),  2, True)
    | c == '$'   = ( (row,col + 1),  2, True)
    | otherwise  = ( (row,col),  3, False)

nextState (row,col) 30 c -- detect comment type
    | c == '/'   = ( (row,col + 1),  40, True) -- single line comment
    | c == '*'   = ( (row,col + 1),  41, True) -- block comments

nextState (row,col) 32 c
    | c == '&'   = ( (row,col + 1),  22,  True)
    | otherwise  = ( (row,col + 1),  999, True)

nextState (row,col) 33 c
    | c == '='   = ( (row,col + 1),  23, True)
    | otherwise  = ( (row,col + 1),  29, True)

nextState (row,col) 40 c -- for single line comments
    | c /= '\n'  = ( (row,col + 1),  40, True) -- ignore everything up to newline
    | otherwise  = ( (row + 1,  0),  50, True) -- return a single TokenComment

nextState (row,col) 41 c -- for block comments, pt. 1
    | c == '\n'  = ( (row + 1,  0),  41, True) --Incriment line counter for block comments
    | c /= '*'   = ( (row,col + 1),  41, True) -- ignore everything up to end of block comment
    | otherwise  = ( (row,col + 1),  42, True) -- continue looking for end of block comment

nextState (row,col) 42 c -- for block comments, pt. 2
    | c == '/'   = ( (row,col + 1),  50, True)  -- return a single TokenComment
    | otherwise  = ( (row,col),  41, False)     -- possibly unclosed block comment, or just found an *
-- Error state
nextState (row,col) _  _   = ( (row,col + 1),  999,True)
