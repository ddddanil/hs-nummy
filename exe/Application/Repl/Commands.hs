module Application.Repl.Commands where

import Nummy.Prelude

data Command = CmdQuit
  deriving (Show, Eq)

commandTable :: [(Text, Command)]
commandTable =
  [ ("quit", CmdQuit)
  ]
