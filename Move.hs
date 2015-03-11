module Move where

import Text.ParserCombinators.Parsec
import Data.Char (toUpper)
import Data.Maybe (fromJust)
import System.IO

data PlayerResponse = Coord (Int, Int) | MetaResponse Option | Invalid
data Option = Pass | Exit | Save deriving Show

data Player = White | Black deriving Eq

instance Show Player where
  show White = "O"
  show Black = "X"

instance Show PlayerResponse where
  show (Coord (x,y)) = numberToLetter x : show (y+1)
  show (MetaResponse o) = show o

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
  return $ Coord (x,y-1)

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
getMove :: Player -> IO PlayerResponse
getMove p = do
  let player = case p of
        Black -> "Black"
        White -> "White"
  response <- prompt $ player ++ ": "
  case parse move "Failed to parse move" response of
   -- Left _ -> putStrLn "Not a valid move." >> getMove p
   Left _ -> return Invalid
   Right m -> return m
