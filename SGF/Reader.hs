module SGF.Reader where

import Text.ParserCombinators.Parsec
import Data.Char

import SGF.Data

type Game String

command :: Parser Elem
command = string "command" >> return Command

metaCommand :: Parser Elem
metaCommand = string "meta" >> return MetaCommand

game :: Parser Game
game = return "foo"

parseGame :: String -> Game
parseGame s = let (Right g) = parse game "failed to parse game" s in g

readGame :: FilePath -> IO Game
readGame f = readFile f >>= \x -> return $ parseGame x
