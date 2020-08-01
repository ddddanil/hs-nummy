module Tests.Parser (
  checkDim, checkUnit, checkQu
, checkParseUnit, checkParseQu, checkParseExpr
) where

import Nummy.Prelude

import Test.Tasty       (TestTree, defaultMain, testGroup, localOption, Timeout(Timeout))
import Test.Tasty.HUnit (testCase, (@?=), (@=?), (@?), Assertion, assertFailure)
import Test.Tasty.ExpectedFailure (expectFail, expectFailBecause)
import Text.Megaparsec
import qualified Data.Text as T (pack, unpack)
import Data.Text.Prettyprint.Doc (Pretty, pretty)

import Nummy.Parser
import Nummy.Metrology
import Nummy.Cache
import Tests.Definitions


-- Parser

getParse :: Parser a -> Label -> CurrencyCache -> IO (Either (ParseErrorBundle Label Void) a)
getParse p s c = runReaderT (runParserT (p <* eof) "<test>" s) c

checkParse :: (Eq a, Pretty a) => Parser a -> TestType -> Label -> a -> CurrencyCache -> Assertion
checkParse p t s x c = do
  res <- getParse p s c
  case res of
    Left err ->
      if t == Succeed
      then assertFailure (errorBundlePretty err)
      else True @? "Expected parse failure"
    Right y -> assert t x y


-- check functions

checkDim :: TestType -> Label -> Dimension -> Dimension -> TestTree
checkDim t s d1 d2 = testCase (T.unpack s) $ assert t d1 d2

checkUnit :: TestType -> Label -> Unit -> Unit -> TestTree
checkUnit t s u1 u2 = testCase (T.unpack s) $ assert t u1 u2

checkQu :: TestType -> Label -> Quantity -> Quantity -> TestTree
checkQu t s q1 q2 = testCase (T.unpack s) $ assert t q1 q2

checkParseUnit :: TestType -> Label -> Unit -> CurrencyCache -> TestTree
checkParseUnit t s u c = testCase (show s) $ checkParse unit t s u c

checkParseQu :: TestType -> Label -> Quantity -> CurrencyCache -> TestTree
checkParseQu t s q c = testCase (show s) $ checkParse quantity t s q c

checkParseExpr :: TestType -> Label -> Quantity -> CurrencyCache -> TestTree
checkParseExpr t s q c = testCase (show s) $ checkParse physical t s q c
