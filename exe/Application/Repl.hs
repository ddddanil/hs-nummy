module Application.Repl (
  ReplAction
, repl
) where

import Nummy.Prelude
import Pipes
import Pipes.Concurrent
import System.IO (hSetEcho, hSetBuffering, BufferMode (NoBuffering))
import System.Console.ANSI

import Nummy.Parser
import Application.Repl.Input
import Application.Repl.Output


type ReplAction = Text -> IO ParserResult

repl :: ReplAction -> IO ()
repl pa = do
  -- Prep
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  setTitle "Nummy"
  -- Setup boxes
  (o1, i1) <- spawn unbounded -- to output
  (o2, i2) <- spawn unbounded -- to parser
  -- Input
  i <- async . runEffect $ runInput >-> toOutput ( o1 <> o2 )
  -- Parser
  p <- async . runEffect $ fromInput i2 >-> runParser pa >-> toOutput o1
  -- Output
  o <- async . runEffect $ fromInput i1 >-> runOutput
  -- run all
  mapM_ wait [i, p, o]


runParser :: ReplAction -> Pipe OutputEvent OutputEvent IO ()
runParser p =
  forever $ do
    e <- await
    case e of
      OPrompt s _ -> do
        res <- liftIO $ p s
        yield $ OResult res
      _ -> return ()

