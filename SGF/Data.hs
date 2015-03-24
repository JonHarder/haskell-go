module SGF.Data where

data Tree = Empty | Branch {commands :: [Command], tree :: Tree}

data MoveCommand = BlackMove | WhiteMove deriving (Show, Eq)
data MetaCommand = Comment | BlackPlace | WhitePlace deriving (Show,Eq)

data Command = MoveCommand | MetaCommand
