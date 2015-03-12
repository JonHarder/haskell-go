module Main where

import qualified System.Console.ANSI as A
-- import Graphics.Vty.Widgets.All

import Board
import Move
import Logic

playerSeq = [Black, White] ++ playerSeq

boardSet :: Board -> Coord -> Player -> MoveResult
boardSet board@(Board b) coord@(Coord (x,y)) p =
  if isCoordOnBoard coord board  then
    if not $ boardGet board coord == Empty then
      Left Occupied
    else
      logic board coord p
  else Left OutOfBounds

main :: IO ()
main = do
  let board = initialBoard $ Nothing
  loop playerSeq board Nothing ""
  where loop :: [Player] -> Board -> Maybe Coord -> String -> IO ()
        loop players b mLastCoord message = do
          A.clearScreen
          A.setCursorPosition 0 0
          putStrLn $ "Message: " ++ message ++ "\n"
          putStrLn $ showBoard b mLastCoord
          putStr "\n"
          -- move :: PlayerResponse
          move <- getMove $ head players
          case move of
           Invalid -> loop (players) b mLastCoord "Invalid move."
           MetaResponse Pass -> loop (tail players) b mLastCoord (show (head players) ++ " passed")
           MetaResponse Exit -> putStrLn "Bye!"
           MetaResponse Save -> loop players b mLastCoord "save feature is not implimented yet"
           Position c -> let result = boardSet b c (head players)
                         in case result of
                             Left Occupied -> loop (players) b mLastCoord "Spaced is already occupied."
                             Left OutOfBounds -> loop players b mLastCoord "Position is off the board."
                             Left Ko -> loop players b mLastCoord "Invalid move due to ko rule."
                             Left Suicide -> loop players b mLastCoord "Move is suicidal."
                             Right b -> loop (tail players) b (Just c) ""
