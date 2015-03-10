module Move (getMove, Player(..)) where

import Text.ParserCombinators.Parsec
import Data.Char (toUpper)
import Data.Maybe (fromJust)
import System.IO

data Point = Point (Int, Int)

data Player = White | Black deriving Show

instance Show Point where
  show (Point (x,y)) = numberToLetter x : show y

letterToNumber :: Char -> Int
letterToNumber c = fromJust $ lookup (toUpper c) (zip ['A'..'Z'] [0..])

numberToLetter :: Int -> Char
numberToLetter = (['A' .. 'Z'] !!)

int :: Parser Int
int = fmap read $ many1 digit

point :: Parser Point
point = do
  x <- fmap letterToNumber $ oneOf $ ['A'..'Z'] ++ ['a'..'z']
  spaces
  y <- int
  return $ Point (x,y)

prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine

-- | Repeatedly prompts player for a game move until a valid point is entered
getMove :: Player -> IO Point
getMove p = do
  move <- prompt $ (show p ++ ": ")
  case parse point "Failed to parse point" move of
   Left _ -> putStrLn "Not a valid move." >> getMove p
   Right point -> return point
