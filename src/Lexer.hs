module Lexer where

import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Language
import qualified Text.Parsec.Token as T

-- The lexer is not a separate phase, but provides useful building blocks
-- There are many other token types in this module, we use only a subset
matlab = emptyDef {
    T.commentLine = "%",
    T.identStart = letter,
    T.identLetter = alphaNum,
    T.reservedNames = ["zeros", "ones", "eye"],
    T.reservedOpNames = ["+", "*", "\\", "-", "="]
}
lexer = T.makeTokenParser matlab

-- The useful building blocks
identifier    = T.identifier    lexer
whiteSpace    = T.whiteSpace    lexer
parens        = T.parens        lexer
integer       = T.integer       lexer
reservedOp    = T.reservedOp    lexer
reserved      = T.reserved      lexer
float         = T.float         lexer
commaSep      = T.commaSep      lexer
natural       = T.natural       lexer -- 1,2,3,...
semi          = T.semi          lexer
-- whiteSpace    = T.whiteSpace    lexer
-- stringLiteral = T.stringLiteral lexer

