
-- Util.hs
--
-- Not a step in compiling.
--
-- Util Module contains AST data structure, (Meggy) Type data structure, TypeSignature data structure, and AVR header and footer
--

module Util where

data AST
    -- Overall structure
    = Prog      AST [AST]           -- Program has a main class and list of classes
    | Class     [AST] String        -- Class has a list of methods and a name
    | MainClass AST                 -- MainClass has only the main method
    | Method    AST String TypeSig  -- Method has a body of statements, method name, and type signature
    | Body      [AST]               -- An if statement, method, while loop etc. have a body of statements (These are blocks {...})
    | Return    AST                 -- Return statements have return expressions, void methods don't have a Return AST

    -- Library Methods
    | SetPixel      AST AST AST     -- A set pixel call has two byte params and a color
    | Delay         AST
    | GetPixel      AST AST
    | CheckButton   AST
    | ToneStart     AST AST

    --Operators
    | LogicalAnd    AST AST
    | LogicalEqual  AST AST
    | LogicalNot    AST
    | Add           AST AST
    | Sub           AST AST
    | Mul           AST AST
    | UnaryMinus    AST
    | LessThan      AST AST

    --ControlFlow
    | If            AST AST AST --Condition, body, optional else
    | While         AST AST     --Condition, body

    --Expression Parents (Byte cast an expression or surround it with parens)
    | ByteCast      AST
    | ParenExp      AST

    | Instance      AST String      -- new XYZ() creates an instance AST which has a class name and 
                                    -- a child that should be invoke
    | Invoke        [AST] String    -- invoke is the child of an instance, it has a method name and a list of expression parameters as children

    --Literals
    | IntLiteral    Int
    | ColorLiteral  Int
    | ButtonLiteral String
    | ToneLiteral   Int
    | Identifier    String
    | Boolean       Bool
    | Epsilon
    deriving (Show,Eq)

data Type
    = BooleanType
    | ByteType
    | IntType
    | VoidType
    | MeggyColorType
    | MeggyButtonType
    | MeggyToneType
    | Error
    deriving (Show,Eq)

data TypeSig
    = TS [(String,Type)] Type       -- A TypeSignature has a list of parameters, which are (Identifier, Type) tuples, and a return type
    deriving(Show,Eq)


-- Provided header and footer code 
-- Written by instructors of CS453, not myself
-- see https://www.cs.arizona.edu/classes/cs453/fall16/more_assignments/PA2/Util.hs
header = 
       "    .file \"main.java\"\n"
    ++ "__SREG__ = 0x3f\n"
    ++ "__SP_H__ = 0x3e\n"
    ++ "__SP_L__ = 0x3d\n"
    ++ "__tmp_reg__ = 0\n"
    ++ "__zero_reg__ = 1\n"
    ++ "    .global __do_copy_data\n"
    ++ "    .global __do_clear_bss\n"
    ++ "    .text\n"
    ++ ".global main\n"
    ++ "    .type   main, @function\n"
    ++ "main:\n"
    ++ "    push r29\n"
    ++ "    push r28\n"
    ++ "    in r28,__SP_L__\n"
    ++ "    in r29,__SP_H__\n"
    ++ "/* prologue: function */\n"
    ++ "    call _Z18MeggyJrSimpleSetupv\n"
    ++ "    /* Need to call this so that the meggy library gets set up */\n\n"

footer = "\n/* epilogue start */\n"
    ++ "    endLabel:\n"
    ++ "    jmp endLabel\n"
    ++ "    ret\n"
    ++ "    .size   main, .-main\n"
