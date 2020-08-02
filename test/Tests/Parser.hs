module Tests.Parser (
  checkDim, checkUnit, checkQu
, checkParseUnit, checkParseQu, checkParseExpr
) where

import Nummy.Prelude

import Control.Monad.Except (liftEither)
import Test.Tasty       (TestTree)
import Test.Tasty.HUnit (testCase, assertFailure)
import Text.Megaparsec
import qualified Data.Text as T (unpack)
import Data.Text.Prettyprint.Doc (Pretty)

import Nummy.Base
import Nummy.Parser
import Nummy.Metrology
import Tests.Definitions


-- Parser

getParse :: Parser a -> Label -> ParseExcept a
getParse p s = do
  c <- ask
  e <- liftIO $ runReaderT (runParserT (p <* eof) "<test>" s) c
  liftEither e

checkParse :: (Eq a, Pretty a) => Parser a -> TestType -> Label -> a -> ParseExcept ()
checkParse p t s x = do
  res <- getParse p s
  assert t x res
  `catchError` \e -> case t of
    Fail -> return () -- Expected parser failure
    _    -> liftIO $ assertFailure (errorBundlePretty e)


-- check functions

checkDim :: TestType -> Label -> Dimension -> Dimension -> TestTree
checkDim t s d1 d2 = testCase (T.unpack s) . runTest $
  withExceptT (const "You can't specify parser failure on a non-parser test" :: e -> Label) $
    assert t d1 d2

checkUnit :: TestType -> Label -> Unit -> Unit -> TestTree
checkUnit t s u1 u2 = testCase (T.unpack s) . runTest $
  withExceptT (const "You can't specify parser failure on a non-parser test" :: e -> Label) $
    assert t u1 u2

checkQu :: TestType -> Label -> Quantity -> Quantity -> TestTree
checkQu t s q1 q2 = testCase (T.unpack s) . runTest $
  withExceptT (const "You can't specify parser failure on a non-parser test" :: e -> Label) $
    assert t q1 q2


checkParseUnit :: TestType -> Label -> Unit -> TestTree
checkParseUnit t s u = testCase (show s) . runTest $ checkParse unit t s u

checkParseQu :: TestType -> Label -> Quantity -> TestTree
checkParseQu t s q = testCase (show s) . runTest $ checkParse quantity t s q

checkParseExpr :: TestType -> Label -> Quantity -> TestTree
checkParseExpr t s q = testCase (show s) . runTest $ checkParse physical t s q

