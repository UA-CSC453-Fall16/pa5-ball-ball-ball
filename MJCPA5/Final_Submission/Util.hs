{- 
            < View in "Raw" on GitHub >
██╗   ██╗████████╗██╗██╗        ██╗  ██╗███████╗
██║   ██║╚══██╔══╝██║██║        ██║  ██║██╔════╝
██║   ██║   ██║   ██║██║        ███████║███████╗
██║   ██║   ██║   ██║██║        ██╔══██║╚════██║
╚██████╔╝   ██║   ██║███████╗██╗██║  ██║███████║
 ╚═════╝    ╚═╝   ╚═╝╚══════╝╚═╝╚═╝  ╚═╝╚══════╝
            < View in "Raw" on GitHub >
-}
--
-- Not a step in compiling.
--
-- Util Module contains AST data structure, (Meggy) Type data structure, TypeSignature data structure, and AVR header and footer
--

module Util where

import Data.Map as M

data AST
    -- Overall structure
    = Prog      AST [AST]           -- Program has a main class and list of classes
    | Class     AST AST String        -- Class has a VarDecl and MethDecl and a name
    | MainClass AST                 -- MainClass has only the main method
    | Method    AST AST String TypeSig  -- Method has a VarDecl, a Body (of statements), method name, and type signature
    | Body      [AST]               -- An if statement, method, while loop etc. have a body of statements (These are blocks {...})
    | VarDecl   [AST]               -- A method and class can start with variable declarations. They will be stored in this list. Each method will
                                    -- have their left ast child be a VarDecl or Epsilon
    | MethDecl  [AST]                    
    | Return    AST                 -- Return statements have return expressions, void methods don't have a Return AST

    -- Library Methods
    | SetPixel      AST AST AST     -- A set pixel call has two byte params and a color
    | Delay         AST
    | GetPixel      AST AST
    | CheckButton   AST
    | ToneStart     AST AST
    | SetAuxLEDs    AST

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

    -- Method call Stuff
    | Instance      String             -- new XYZ() creates an instance AST which has a class name
    | Invoke        AST [AST] String   -- invoke is the parent of an instance, it has a receiver (left), list of expression parameters, and method name as children

    -- PA5 Stuff
    | IntArrayInstance AST          -- Child node is the capacity which may be an expression which needs to be evaluated
    | ColorArrayInstance AST        -- Child node is the capacita ahasi ahasf adwn 42 is the answer to the universe weba
    | Variable      Type String     -- A declaration for a variable -> Type Id;
    | Assignment    String AST      -- The ast is the value of the expression being assigned to the identifier represented by String -> Id = E;
    | ArrayAssignment String AST AST -- The identifier of the array is the String, the left AST is the expression for the index of the array 
                                     -- and the right AST is the value being assigned to that array index. -> Id [E] = E;
    | ArrayAccess    AST AST          -- The left AST is the array being accessed, the right AST is the index -> E[E]
    | ArrayLength    AST 

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
    | IntArrayType
    | ColorArrayType
    | ClassType String
    | Error
    deriving (Show,Eq)

data TypeSig
    = TS [(String,Type)] Type       -- A TypeSignature has a list of parameters, which are (Identifier, Type) tuples, and a return type
    deriving(Show,Eq)

-- SymbolTable data structure
-- Scope is outermost program scope and the stack of strings keeps track
-- of our current scope nesting.
data SymbolTable = SymTab Scope [String]
    deriving (Show,Eq)

type Scope = M.Map String STE

data STE
    =   ClassSTE    String Scope Int -- String is name, int is the max offset for member variables
    |   MethodSTE   String (Scope, TypeSig) Int -- String is name, int is the max offset for params and local variables
    |   VarSTE      Type String String Int -- Type, Name, Base, Offset
    deriving (Show,Eq)

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
