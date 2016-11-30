
-- Relex.hs
--
-- Step (2) of compiling.
--
-- ReLex module simplifies some of the tokens by condensing Meggy call tokens and Byte Casting
--

module ReLex where

import Lexer

--Combines token sequences for more logical parsing
-- Example: [TokenBigMeggy, TokenDot, TokenSetPixel] -> TokenMeggySetPix
tokenSimplifier :: [(Token, (Int,Int))] -> [(Token, (Int,Int))]
tokenSimplifier ((TokenBigMeggy, (r1,c1)):(TokenDot,(_,_)):(TokenSetPixel,(_,_)):rest)                = (TokenMeggySetPix, (r1,c1)):(tokenSimplifier rest)
tokenSimplifier ((TokenBigMeggy, (r1,c1)):(TokenDot,(_,_)):(TokenToneStart,(_,_)):rest)               = (TokenMeggyToneStart, (r1,c1)):(tokenSimplifier rest)
tokenSimplifier ((TokenBigMeggy, (r1,c1)):(TokenDot,(_,_)):(TokenDelay,(_,_)):rest)                   = (TokenMeggyDelay, (r1,c1)):(tokenSimplifier rest)
tokenSimplifier ((TokenBigMeggy, (r1,c1)):(TokenDot,(_,_)):(TokenGetPixel,(_,_)):rest)                = (TokenMeggyGetPix, (r1,c1)):(tokenSimplifier rest)
tokenSimplifier ((TokenBigMeggy, (r1,c1)):(TokenDot,(_,_)):(TokenCheckButton,(_,_)):rest)             = (TokenMeggyCheckButton, (r1,c1)):(tokenSimplifier rest)

tokenSimplifier ((TokenBigMeggy, (r1,c1)):(TokenDot,(_,_)):(TokenColor,(_,_)):(TokenDot,(_,_)):rest)  = (TokenMeggyColor, (r1,c1)):(tokenSimplifier rest)
tokenSimplifier ((TokenBigMeggy, (r1,c1)):(TokenDot,(_,_)):(TokenButton,(_,_)):(TokenDot,(_,_)):rest) = (TokenMeggyButton, (r1,c1)):(tokenSimplifier rest)
tokenSimplifier ((TokenBigMeggy, (r1,c1)):(TokenDot,(_,_)):(TokenTone,(_,_)):(TokenDot,(_,_)):rest)   = (TokenMeggyTone, (r1,c1)):(tokenSimplifier rest)

--TODO Check that this isn't an issue
tokenSimplifier ((TokenBigMeggy, (r1,c1)):(TokenDot,(_,_)):(TokenColor,(_,_)):rest)  = (TokenMeggyColorType, (r1,c1)):(tokenSimplifier rest)
tokenSimplifier ((TokenBigMeggy, (r1,c1)):(TokenDot,(_,_)):(TokenButton,(_,_)):rest) = (TokenMeggyButtonType, (r1,c1)):(tokenSimplifier rest)
tokenSimplifier ((TokenBigMeggy, (r1,c1)):(TokenDot,(_,_)):(TokenTone,(_,_)):rest)   = (TokenMeggyToneType, (r1,c1)):(tokenSimplifier rest)


tokenSimplifier ((TokenImport, (r1,c1)):
                 (TokenLittleMeggy, (_,_)):
                 (TokenDot,(_,_)):
                 (TokenBigMeggy,(_,_)):
                 (TokenSemiColon,(_,_)):rest)  = (TokenMeggyImport, (r1,c1)):(tokenSimplifier rest)

tokenSimplifier ((TokenLeftParen, (r1,c1)):(TokenByte,(_,_)):(TokenRightParen,(_,_)):rest)            = (TokenByteCast, (r1,c1)):(tokenSimplifier rest)

tokenSimplifier (x:rest) = x:(tokenSimplifier rest)
tokenSimplifier [] = []
