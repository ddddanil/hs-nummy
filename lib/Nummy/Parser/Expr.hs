module Nummy.Parser.Expr (

) where

import Protolude hiding (Prefix, Infix, try)
import Data.String (String)
import Data.Maybe (fromJust)
import Data.Ratio (approxRational)
import Text.Parsec as P hiding ( (<|>) )
import Text.Parsec.Char as P.Char
import Text.Parsec.Expr as P.Expr
import Text.ParserCombinators.Parsec.Number as P.Number (floating2)
import qualified Text.PrettyPrint.Leijen as PP

import Nummy.Parser.Base
import Nummy.Parser.Unit
import Nummy.Metrology.Definitions hiding (length, mass, time)
import Nummy.Metrology.Dimension as D
import Nummy.Metrology.Quantity as Q
import Nummy.Metrology.Unit as U

-- Quantity expr parser

-- exprTable :: OpTable Quantity
-- exprTable =
