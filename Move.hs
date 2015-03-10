module Move where

import Text.ParserCombinators.Parsec
import Data.Char (toUpper)
import Data.Maybe (fromJust)
import System.IO

data Coord = Coord (Int, Int)

data Player = White | Black deriving Eq

instance Show Player where
  show White = "O"
  show Black = "X"

instance Show Coord where
  show (Coord (x,y)) = numberToLetter x : show y

letterToNumber :: Char -> Int
letterToNumber c = fromJust $ lookup (toUpper c) (zip ['A'..'Z'] [0..])

numberToLetter :: Int -> Char
numberToLetter = (['A' .. 'Z'] !!)

int :: Parser Int
int = fmap read $ many1 digit

coord :: Parser Coord
coord = do
  x <- fmap letterToNumber $ oneOf $ ['A'..'Z'] ++ ['a'..'z']
  spaces
  y <- int
  return $ Coord (x,y)

prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine

-- | Repeatedly prompts player for a game move until a valid coord is entered
getMove :: Player -> IO Coord
getMove p = do
  let player = case p of
        Black -> "Black"
        White -> "White"
  move <- prompt $ player ++ ": "
  case parse coord "Failed to parse coord" move of
   Left _ -> putStrLn "Not a valid move." >> getMove p
   Right coord -> return coord
