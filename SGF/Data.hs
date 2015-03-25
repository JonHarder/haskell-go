module SGF.Data where

data Tree = Empty | Branch {commands :: [Command], tree :: Tree}

newtype Location = L (Int,Int) deriving (Show, Eq)
data MoveCommand = BlackMove Location | WhiteMove Location deriving (Show, Eq)
data MetaCommand = Comment String | BlackPlace Location | WhitePlace Location deriving (Show,Eq)

data Command = Move MoveCommand | Meta MetaCommand deriving Show
