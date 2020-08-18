module Tests.Definitions (
  TestType(..), assert
, short_timeout, average_timeout, long_timeout
) where

import Nummy.Prelude hiding (length, second)

import Control.Monad.Fail (fail)
import Test.Tasty (Timeout(Timeout))
import Test.Tasty.HUnit ((@?), assertFailure, Assertion)
import Data.Text.Prettyprint.Doc

import Nummy.Parser
import Nummy.Cache (ReadCache, runReadCache)


-- TestType

data TestType = Equal | NotEqual | Fail
  deriving (Show, Eq, Ord)

assert :: (Eq a, Pretty a) => TestType -> a -> a -> Assertion
assert t x y =
  case t of
    Equal ->    x == y @? ( show $ (pretty x) <+> "==" <+> (pretty y) )
    NotEqual -> x /= y @? ( show $ (pretty x) <+> "/=" <+> (pretty y) )
    Fail ->     assertFailure $ "Expected failure, got " ++ ( show $ (pretty x) <+> (pretty y) )


-- Timeouts

short_timeout :: Timeout
short_timeout = Timeout 50 "50ms"

average_timeout :: Timeout
average_timeout = Timeout 333 "1/3s"

long_timeout :: Timeout
long_timeout = Timeout 1000 "1s"

