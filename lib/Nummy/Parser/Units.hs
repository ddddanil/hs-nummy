module Nummy.Parser.Units where

import Protolude hiding (Prefix, Infix)
import Text.Parsec as P hiding ( (<|>) )
import Text.Parsec.Char as P.Char
import Text.Parsec.Prim as P.Prim hiding ( (<|>) )
import Text.Parsec.Combinator as P.Comb
import Text.Parsec.Expr as P.Expr

import Nummy.Metrology.Definitions (symbol_table)

expr    = buildExpressionParser table term
        <?> "expression"

term    =  parens expr
        <|> natural
        <?> "simple expression"

table   = [ 
          ]

