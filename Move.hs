module Move where

import Text.ParserCombinators.Parsec
import Data.Char (toUpper)
import Data.Maybe (fromJust)
import System.IO

data PlayerResponse = Position Coord | MetaResponse Option | Invalid
data Coord = Coord (Int, Int) deriving (Eq, Ord)
data Option = Pass | Exit | Save deriving Show

data Player = White | Black deriving Eq

instance Show Player where
  show White = "O"
  show Black = "X"

instance Show PlayerResponse where
  show (Position c) = show c
  show (MetaResponse o) = show o
  show Invalid = "Invalid"

instance Show Coord where
  show (Coord (x,y)) = numberToLetter x : show (y+1)

letterToNumber :: Char -> Int
letterToNumber c = fromJust $ lookup (toUpper c) (zip ['A'..'Z'] [0..])

numberToLetter :: Int -> Char
numberToLetter = (['A' .. 'Z'] !!)

int :: Parser Int
int = fmap read $ many1 digit

coord :: Parser PlayerResponse
coord = do
  x <- fmap letterToNumber $ oneOf $ ['A'..'Z'] ++ ['a'..'z']
  spaces
  y <- int
  return $ Position $ Coord (x,y-1)

pass :: Parser Option
pass = choice [string "pass", string "Pass"] >> return Pass

exit :: Parser Option
exit = choice [string "exit", string "Exit"] >> return Exit

save :: Parser Option
save = choice [string "save", string "Save"] >> return Save

metaCommand :: Parser PlayerResponse
metaCommand = fmap MetaResponse $ choice [pass,exit,save]

move :: Parser PlayerResponse
move = try coord <|> metaCommand

prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine

-- | Repeatedly prompts player for a game move until a valid coord or metacommand is entered
getMove :: (Player, Int) -> IO PlayerResponse
getMove str@(p, num) = do
  let player = case p of
        Black -> "Black"
        White -> "White"
  response <- prompt $ player ++ "(" ++ show num ++ "): "
  case parse move "Failed to parse move" response of
   -- Left _ -> putStrLn "Not a valid move." >> getMove str
   Left _ -> return Invalid
   Right m -> return m
