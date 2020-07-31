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
import Tests.Definitions


-- Parser

getParse :: Parser a -> Label -> Either (ParseErrorBundle Label Void) a
getParse p s = parse (p <* eof) "<test>" s

checkParse :: (Eq a, Pretty a) => Parser a -> TestType -> Label -> a -> Assertion
checkParse p t s x =
  case getParse p s of
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

checkParseUnit :: TestType -> Label -> Unit -> TestTree
checkParseUnit t s u = testCase (show s) $ checkParse unit t s u

checkParseQu :: TestType -> Label -> Quantity -> TestTree
checkParseQu t s q = testCase (show s) $ checkParse quantity t s q

checkParseExpr :: TestType -> Label -> Quantity -> TestTree
checkParseExpr t s q = testCase (show s) $ checkParse physical t s q
