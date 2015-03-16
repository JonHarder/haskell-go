module SGF.Data where

data Game = GoMetaCommand
          | GoCommand
          | Game deriving (Show, Eq)
