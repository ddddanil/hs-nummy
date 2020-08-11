module Tests.Definitions (
  ParseExcept
, runTest
, TestType(..), assert
, short_timeout, average_timeout, long_timeout
) where

import Nummy.Prelude hiding (length, second)

import Control.Monad.Fail (fail)
import Test.Tasty (Timeout(Timeout))
import Test.Tasty.HUnit ((@?), assertFailure)
import Data.Text.Prettyprint.Doc

import Nummy.Parser
import Nummy.Cache (ReadCache, runReadCache)


-- Test monad

type ParseExcept = ExceptT ParserError ReadCache

runTest :: (Show e) => ExceptT e ReadCache a -> IO a
runTest m = do
  ex <- runReadCache . runExceptT $ m
  case ex of
    Right x -> return x
    Left err -> fail ("Unhandled parser exception:\n" ++ show err)


-- TestType

data TestType = Equal | NotEqual | Fail
  deriving (Show, Eq, Ord)

assert :: (Eq a, Pretty a) => TestType -> a -> a -> ParseExcept ()
assert t x y =
  case t of
    Equal ->    liftIO $ x == y @? ( show $ (pretty x) <+> "==" <+> (pretty y) )
    NotEqual -> liftIO $ x /= y @? ( show $ (pretty x) <+> "/=" <+> (pretty y) )
    Fail -> liftIO . assertFailure $ "Expected failure, got " ++ ( show $ (pretty x) <+> (pretty y) )


-- Timeouts

short_timeout :: Timeout
short_timeout = Timeout 50 "50ms"

average_timeout :: Timeout
average_timeout = Timeout 333 "1/3s"

long_timeout :: Timeout
long_timeout = Timeout 1000 "1s"

