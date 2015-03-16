module SGF.Reader where

import Text.ParserCombinators.Parsec

import SGF.Data

game :: Parser Game
game = return GoCommand

parseGame :: String -> Game
parseGame s = let (Right g) = parse game "failed to parse game" s in g

readGame :: FilePath -> IO Game
readGame f = readFile f >>= \x -> return $ parseGame x
