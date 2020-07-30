module Tests.Definitions (
  TestType(..), assert
, short_timeout, average_timeout, long_timeout
) where

import Nummy.Prelude hiding (length, second)
import Data.Maybe (fromJust)

import Test.Tasty (Timeout(Timeout))
import Test.Tasty.HUnit (Assertion, (@?))
import qualified Text.PrettyPrint.Leijen as PP

import Nummy.Metrology as M


-- TestType

data TestType = Fail | Succeed deriving (Show, Eq, Ord)

assert :: (Eq a, PP.Pretty a) => TestType -> a -> a -> Assertion
assert Succeed a b = a == b @? "assert equal\n" ++ (show . PP.pretty $ a) ++ " == " ++ (show . PP.pretty $ b)
assert Fail    a b = a /= b @? "assert not equal\n" ++ (show . PP.pretty $ a) ++ " /= " ++ (show . PP.pretty $ b)


-- Timeouts

short_timeout = Timeout 50 "50ms"
average_timeout = Timeout 333 "1/3s"
long_timeout = Timeout 1000 "1s"

