module Application.Repl.Parser (runParser) where

import Nummy.Prelude
import Pipes
import Nummy.Parser
import Nummy.Cache
import Nummy.Metrology
import Nummy.Currency
import Nummy.Metrology.Definitions
import Application.Repl.Output
import Application.Repl.Commands

runParser :: Pipe OutputEvent OutputEvent ReadCache ()
runParser = do
  lookupUnit <- lift lookupUnit'
  forever $ do
    e <- await
    case e of
      OPrompt s _ -> do
        let res = nummy (ParserOptions lookupUnit lookupCommand) s
        yield $ OResult res
      _ -> return ()

lookupUnit' :: ReadCache (Text -> Maybe Unit)
lookupUnit' = do
  currencyTable <- getCurrency
  return $ \u -> lookup u unitTable <|> lookup u currencyTable

lookupCommand :: Text -> Maybe Command
lookupCommand c = lookup c commandTable
