
-- Dot.hs
--
-- Not a step in compiling. Used for visualizing the AST after Parser.hs
--

-- Disclaimer: This code belongs to / has been adapted from the PA3 submission with ID 3881

{-
    NOTE FOR USAGE: Dot does not play well with '"' characters. Method signatures
    store params as strings, as such if you generate an AST of a MeggyJava program with
    classes and methods you need to manually replace inner '"'s with '\"' in every 
    label = "(-->here<--)".
-}


module Dot where

import Util

genDot :: AST -> String
genDot ast =
    let (_,dotstr) = astdotviz_rec 0 1 ast
    in "digraph {\n" ++ dotstr ++"}"

astdotviz_rec :: Int -> Int -> AST -> (Int,String)
astdotviz_rec _ myid astnode =
    let
        prefix = (show myid) ++ "  [label = \""
        suffix = "\"];\n"
    in
        case astnode of
            (Prog ast1 ast2) ->
                let 
                    (maxChild,subTreeStr) = visitChildren myid (myid+1) (ast1:ast2)
                in 
                    (maxChild,subTreeStr++prefix++"Prog"++suffix)

            (Class ast1 ast2) ->
                let 
                    (maxChild,subTreeStr) = visitChildren myid (myid+1) (ast1)
                in 
                    (maxChild,subTreeStr++prefix++"Class " ++ ast2 ++suffix)

            (MainClass ast1) ->
                let 
                    (maxChild,subTreeStr) = visitChildren myid (myid+1) (ast1:[])
                in 
                    (maxChild,subTreeStr++prefix++"MainClass"++suffix)

            (Method ast1 str typesig) ->
                let 
                    (maxChild,subTreeStr) = visitChildren myid (myid+1) (ast1:[])
                in 
                    (maxChild,subTreeStr++prefix++"Method " ++ str ++ " typesig: " ++ (show typesig) ++ suffix)

            (Body ast1) ->
                let 
                    (maxChild,subTreeStr) = visitChildren myid (myid+1) (ast1)
                in 
                    (maxChild,subTreeStr++prefix++"Body"++suffix)

            (Return ast1) ->
                let 
                    (maxChild,subTreeStr) = visitChildren myid (myid+1) (ast1:[])
                in 
                    (maxChild,subTreeStr++prefix++"Return"++suffix)

            (SetPixel ast1 ast2 ast3) ->
                let 
                    (maxChild,subTreeStr) = visitChildren myid (myid+1) (ast1:ast2:ast3:[])
                in
                    (maxChild,subTreeStr++prefix++"SetPixel"++suffix)

            (Delay ast1) ->
                let 
                    (maxChild,subTreeStr) = visitChildren myid (myid+1) (ast1:[])
                in
                    (maxChild,subTreeStr++prefix++"Delay"++suffix)

            (GetPixel ast1 ast2) ->
                let 
                    (maxChild,subTreeStr) = visitChildren myid (myid+1) (ast1:ast2:[])
                in
                    (maxChild,subTreeStr++prefix++"GetPixel"++suffix)

            (CheckButton ast1) ->
                let 
                    (maxChild,subTreeStr) = visitChildren myid (myid+1) (ast1:[])
                in
                    (maxChild,subTreeStr++prefix++"CheckButton"++suffix)

            (ToneStart ast1 ast2) ->
                let 
                    (maxChild,subTreeStr) = visitChildren myid (myid+1) (ast1:ast2:[])
                in
                    (maxChild,subTreeStr++prefix++"ToneStart"++suffix)

            (If ast1 ast2 ast3) ->
                let 
                    (maxChild,subTreeStr) = visitChildren myid (myid+1) (ast1:ast2:ast3:[])
                in
                    (maxChild,subTreeStr++prefix++"If"++suffix)

            (While ast1 ast2) ->
                let 
                    (maxChild,subTreeStr) = visitChildren myid (myid+1) (ast1:ast2:[])
                in
                    (maxChild,subTreeStr++prefix++"While"++suffix)

            (ByteCast ast) ->
                let 
                    (maxChild,subTreeStr) = visitChildren myid (myid+1) (ast:[])
                in
                    (maxChild,subTreeStr++prefix++"ByteCast"++suffix)

            (ParenExp ast) ->
                let 
                    (maxChild,subTreeStr) = visitChildren myid (myid+1) (ast:[])
                in
                    (maxChild,subTreeStr++prefix++"ParenExp"++suffix)

            (Add ast1 ast2) ->
                let 
                    (maxChild,subTreeStr) = visitChildren myid (myid+1) (ast1:ast2:[])
                in
                    (maxChild,subTreeStr++prefix++"Add"++suffix)

            (Sub ast1 ast2) ->
                let 
                    (maxChild,subTreeStr) = visitChildren myid (myid+1) (ast1:ast2:[])
                in
                    (maxChild,subTreeStr++prefix++"Sub"++suffix)

            (LogicalEqual ast1 ast2) ->
                let 
                    (maxChild,subTreeStr) = visitChildren myid (myid+1) (ast1:ast2:[])
                in
                    (maxChild,subTreeStr++prefix++"=="++suffix)

            (LogicalNot ast1) ->
                let 
                    (maxChild,subTreeStr) = visitChildren myid (myid+1) (ast1:[])
                in
                    (maxChild,subTreeStr++prefix++"LogicalNot"++suffix)

            (LessThan ast1 ast2) ->
                let 
                    (maxChild,subTreeStr) = visitChildren myid (myid+1) (ast1:ast2:[])
                in
                    (maxChild,subTreeStr++prefix++"<"++suffix)

            (LogicalAnd ast1 ast2) ->
                let 
                    (maxChild,subTreeStr) = visitChildren myid (myid+1) (ast1:ast2:[])
                in
                    (maxChild,subTreeStr++prefix++"&&"++suffix)

            (UnaryMinus ast) ->
                let 
                    (maxChild,subTreeStr) = visitChildren myid (myid+1) (ast:[])
                in
                    (maxChild,subTreeStr++prefix++"UnaryMinus"++suffix)

            (Mul ast1 ast2) ->
                let 
                    (maxChild,subTreeStr) = visitChildren myid (myid+1) (ast1:ast2:[])
                in
                    (maxChild,subTreeStr++prefix++"Mul"++suffix)

            (Invoke ast1 ast2) ->
                let 
                    (maxChild,subTreeStr) = visitChildren myid (myid+1) (ast1)
                in
                    (maxChild,subTreeStr++prefix++" Invoke " ++ ast2 ++ "  " ++suffix)

            (Instance ast1 ast2) ->
                let 
                    (maxChild,subTreeStr) = visitChildren myid (myid+1) (ast1:[])
                in
                    (maxChild,subTreeStr++prefix++"Instance " ++ ast2 ++ "  " ++suffix)

            (IntLiteral x) -> (myid, prefix++"IntLiteral "++(show x)++suffix)

            (ColorLiteral x) -> (myid, prefix++"ColorLiteral "++(show x)++suffix)

            (Boolean x) -> (myid, prefix++" Boolean (" ++ show x ++") "++suffix)

            (ToneLiteral x) -> (myid, prefix++" ToneLiteral (" ++ show x ++") "++suffix)

            (ButtonLiteral x) -> (myid, prefix++" ButtonLiteral (" ++ show x ++") "++suffix)

            (Identifier x) -> (myid, prefix++" Id (" ++ show x ++") "++suffix)

            (Epsilon) -> (myid, prefix++"(Epsilon - Ignore)"++suffix)

            --x -> error ("Found " ++ show x ++ " but did not match any pattern\n") 

visitChildren :: Int -> Int -> [AST] -> (Int,String)
visitChildren _ myid [] = (myid,"")
visitChildren pid myid (x:xs) =
    let (xmax,xstr) = astdotviz_rec pid myid x
        (maxid,str) = visitChildren pid (xmax+1) xs
    in  (maxid,xstr++str ++ show pid ++" -> "++ show myid ++ "\n" )
