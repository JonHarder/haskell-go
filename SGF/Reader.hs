module SGF.Reader where

import Text.ParserCombinators.Parsec
import Data.Char
import Data.Maybe (fromJust)
import Data.List (elemIndex)

import SGF.Data

type Game = String

brackets = between (char '[') (char ']')

blackMove :: Parser MoveCommand
blackMove = do
    oneOf "bB"
    loc <- brackets location
    return $ BlackMove $ L loc

whiteMove :: Parser MoveCommand
whiteMove = do
    oneOf "wW"
    loc <- brackets location
    return $ WhiteMove $ L loc

comment :: Parser MetaCommand
comment = do
    oneOf "cC"
    s <- brackets $ many1 $ noneOf "]"
    return $ Comment s

addWhite :: Parser MetaCommand
addWhite = do
    string "AW"
    loc <- brackets location
    return $ WhitePlace $ L loc

addBlack :: Parser MetaCommand
addBlack = do
    string "AB"
    loc <- brackets location
    return $ BlackPlace $ L loc

metaCommand :: Parser MetaCommand
metaCommand = try addBlack <|> try addWhite <|> comment

moveCommand :: Parser MoveCommand
moveCommand = whiteMove <|> blackMove

command :: Parser Command
command = fmap Move moveCommand <|> fmap Meta metaCommand

location :: Parser (Int, Int)
location = do
    x <- letter
    y <- letter
    return (letterNum x, letterNum y)

letterNum :: Char -> Int
letterNum l = fromJust $ elemIndex (toUpper l) ['A' .. 'Z']

game :: Parser Game
game = return "foo"

parseGame :: String -> Game
parseGame s = let (Right g) = parse game "failed to parse game" s in g

readGame :: FilePath -> IO Game
readGame f = readFile f >>= \x -> return $ parseGame x
