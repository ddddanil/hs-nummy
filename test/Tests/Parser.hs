module Tests.Parser (
  checkDim, checkUnit, checkQu
, checkParseUnit, checkParseQu, checkParseExpr
) where

import Nummy.Prelude

import Control.Monad.Except (liftEither)
import Test.Tasty       (TestTree)
import Test.Tasty.HUnit (testCase, assertFailure, Assertion)
import Text.Megaparsec
import qualified Data.Text as T (unpack)
import Data.Text.Prettyprint.Doc (Pretty)

import Nummy.Parser
import Nummy.Metrology
import Nummy.Metrology.Definitions
import Tests.Definitions


-- Parser

getParse :: Parser () a -> Text -> Either ParserError a
getParse p s =
  runReader (runParserT (p <* eof) "<test>" s)
    $ (ParserOptions lookupUnit (const Nothing))
  where
    lookupUnit u = lookup u unitTable

checkParse :: (Eq a, Pretty a) => Parser () a -> TestType -> Text -> a -> Assertion
checkParse p t s x = do
  case getParse p s of
    Left e -> case t of
      Fail -> return () -- Expected parser failure
      _    -> assertFailure (errorBundlePretty e)
    Right res -> assert t x res


-- check functions

checkDim :: TestType -> Text -> Dimension -> Dimension -> TestTree
checkDim t s d1 d2 = testCase (T.unpack s) $
    assert t d1 d2

checkUnit :: TestType -> Text -> Unit -> Unit -> TestTree
checkUnit t s u1 u2 = testCase (T.unpack s) $
    assert t u1 u2

checkQu :: TestType -> Text -> Quantity -> Quantity -> TestTree
checkQu t s q1 q2 = testCase (T.unpack s) $
    assert t q1 q2


checkParseUnit :: TestType -> Text -> Unit -> TestTree
checkParseUnit t s u = testCase (show s) $ checkParse unit t s u

checkParseQu :: TestType -> Text -> Quantity -> TestTree
checkParseQu t s q = testCase (show s) $ checkParse quantity t s q

checkParseExpr :: TestType -> Text -> Quantity -> TestTree
checkParseExpr t s q = testCase (show s) $ checkParse physical t s q

