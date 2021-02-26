module Parser where

import Lexer -- We only use a few features of the Lexer
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr hiding (Operator)

run_parser :: Parser a -> String -> a
run_parser p str = case parse p "source name" str of
    Left error_string -> error ("parse error: " ++ show error_string)
    Right parsed -> parsed 

-- A Matlab program consists of
-- a list of statements separated by semicolons or newlines
-- If the semicolon is missing from the statement, the result of
-- the expression is printed to standard out
-- Statements consist only of the form V = E; where V is a variable
-- and E is an expression or initialization statement
data Statement = Assign Variable Expression Print
    deriving Show
type Matlab = [Statement]
data Print = Output | Silent
    deriving Show
type Variable = String

data Expression =
      Initialize Initializer Dims -- zeros(n,m) or ones(n,1) or eye(n,n)
    | UnaryOp  Operator Expression
    | BinaryOp Operator Expression Expression
    | Variable String
    | Scalar   Double
    deriving Show
data Operator =
      Times      -- A*B, matrix-matrix or matrix-vector product
    | Minus      -- A-B, elementwise subtraction
    | Plus       -- A+B, elementwise addition
    | Solve      -- A\b, solving the linear system Ax = b
    | Transpose  -- A', transpose of matrix or vector
    deriving Show
data Initializer = Zeros | Ones | Eye
instance Show Initializer where
    show Zeros = "zeros"
    show Ones = "ones"
    show Eye = "eye"

type Dims = (Integer, Integer)

expr_parser :: Parser Expression
expr_parser = buildExpressionParser expr_table term <?> "expression"
expr_table = [
    [postfix  "'"  (UnaryOp Transpose)],
    [
        binary "*"  (BinaryOp Times) AssocLeft,
        binary "\\" (BinaryOp Solve) AssocLeft
    ],
    [
        binary "+"  (BinaryOp Plus)  AssocLeft,
        binary "-"  (BinaryOp Minus) AssocLeft
    ] ]

initializer :: Parser Initializer
initializer =
        (reserved "zeros" >> return Zeros)
    <|> (reserved "ones" >> return Ones)
    <|> (reserved "eye" >> return Eye)
    <?> "zeros, ones, eye"

term = parens expr_parser
    <|> do
        i <- initializer
        args <- parens $ commaSep $ natural
        case length args of
            1 -> return (Initialize i ((args !! 0), (args !! 0)))
            2 -> return (Initialize i ((args !! 0), (args !! 1)))
            _ -> error "Provide shape as (N) for NxN or (M,N) for MxN"
    <|> fmap Scalar float
    <|> fmap Variable identifier
    <?> "variable, initializer, or numeric scalar"
binary op fun assoc = Infix (do{ reservedOp op; return fun }) assoc
postfix op fun = Postfix (do{ reservedOp op; return fun })

statement :: Parser Statement
statement = do
    l <- identifier
    spaces >> char '=' >> spaces
    e <- expr_parser
    spaces
    print <- (semi >> return Silent) <|> (return Output)
    return (Assign l e print)

-- IDEA: Make a comment a "statement", inserting it in the .c file to help
--       annotate it
comment = char '%' >> many (noneOf "\n\r") >> endOfLine

parser :: Parser Matlab
parser = do
    spaces
    -- TODO: Allow comments in places besides the beginning of the program!
    many comment
    ss <- many statement
    eof
    return ss

