module Tests.Definitions (
  TestType(..), assert
, short_timeout, average_timeout, long_timeout
) where

import Nummy.Prelude hiding (length, second)

import Test.Tasty (Timeout(Timeout))
import Test.Tasty.HUnit (Assertion, (@?))
import Data.Text.Prettyprint.Doc


-- TestType

data TestType = Fail | Succeed deriving (Show, Eq, Ord)

assert :: (Eq a, Pretty a) => TestType -> a -> a -> Assertion
assert Succeed a b = a == b @? "assert equal\n" ++ (show . pretty $ a) ++ " == " ++ (show . pretty $ b)
assert Fail    a b = a /= b @? "assert not equal\n" ++ (show . pretty $ a) ++ " /= " ++ (show . pretty $ b)


-- Timeouts

short_timeout :: Timeout
short_timeout = Timeout 50 "50ms"

average_timeout :: Timeout
average_timeout = Timeout 333 "1/3s"

long_timeout :: Timeout
long_timeout = Timeout 1000 "1s"

