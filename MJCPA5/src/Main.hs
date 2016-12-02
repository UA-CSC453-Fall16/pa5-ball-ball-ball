
-- SymbolTable.hs
--
-- Step (0) of compiling. Entry point of program.
--
-- Main Module 
--

module Main where

import System.IO
import System.Environment
import Lexer
import ReLex
import Parser
import Dot
--import BuildST
--import TypeCheck
--import AVRGen


main :: IO ()
main = do
    -- get input
    [inf_name] <- getArgs
    in_file <- openFile inf_name ReadMode
    raw_input <- hGetContents in_file

    -- create tokens (Lexer.hs)
    let tokens1 = lexer raw_input

    -- simplify tokens with second lexing pass (ReLex.hs)
    let final_token_string = tokenSimplifier tokens1
    
    putStrLn ("\nTwittling thumbs... \n\t -> Lexing Done.")

    -- parse tokens and build AST (Parser.hs)
    let ast = genAST final_token_string

    putStrLn ("Arranging Skittles by Color...\n\t -> AST Generated.")

    -- create .dot ouput for visualization (Dot.hs)
    let dot = genDot ast
    let outf_name = inf_name ++ ".AST.dot"
    out_file <- openFile outf_name WriteMode
    hPutStrLn out_file dot
    hClose out_file --AST Dot debug output
    putStrLn ("Checking Piazza... \n\t -> AST dot file generated")

{- start of omit shit because we are testing" comment

    -- traverses AST and builds a symbol table (BuildST.hs)
    let symbol_table = genSymbolTable ast

    -- uses AST and symbol table to perform type checking (TypeCheck.hs)
    if typeCheck (ast, symbol_table) then
        putStrLn ("Your MeggyJr must be so proud! :,-)\n\t -> Type Checking Passed.")
    else
        putStrLn ("Type Checking Failed - Be Better\n") --Should never execute if type checker throws error 
                                                      -- -> you and your code are beautiful. 

    -- uses AST and symbol table to generate AVR code (AVRGen.hs)
    let (label, avr_code) = avrCodeGen (ast, symbol_table) 0 
    putStrLn ("Counting Nibbles between the Frame Pointer and Stack Pointer... \n\t -> AVR Code Generated.")

    ---write output
    let outf_name = inf_name ++ ".s"
    out_file <- openFile outf_name WriteMode
    hPutStrLn out_file avr_code
    hClose out_file
    putStrLn ("You win this time...\n\t -> AVR Code written to file: " ++ outf_name ++ "\nBye Weirdo!\n\n")


    End of "omit shit because we are testing" comment -}