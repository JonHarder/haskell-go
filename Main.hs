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

newline :: IO ()
newline = putStr "\n"

main :: IO ()
main = do
  let board = initialBoard $ Nothing
  loop playerSeq board Nothing (0,0,1) ""
  -- loop takes the infinite player sequence, the board, the triplet of black captured, white captured, and move number and a message from the last round
  where loop :: [Player] -> Board -> Maybe Coord -> (Int,Int,Int) -> String -> IO ()
        loop players b mLastCoord (blackCaptured, whiteCaptured, moveNum) message = do
          let retry err = loop players b mLastCoord (blackCaptured, whiteCaptured, moveNum) err
          A.clearScreen
          A.setCursorPosition 0 0
          putStrLn $ " Black Captured: " ++ show blackCaptured
          putStrLn $ " White Captured: " ++ show whiteCaptured
          newline
          putStrLn $ " Message: " ++ message ++ "\n"
          -- putStrLn $ showBoard b mLastCoord
          print b
          newline
          move <- getMove (head players, moveNum)
	  newline
          case move of
           Invalid -> retry "Invalid move."
           MetaResponse Pass -> loop (tail players) b mLastCoord (blackCaptured, whiteCaptured, moveNum+1) (show (head players) ++ " passed")
           MetaResponse Exit -> putStrLn "Bye!"
           MetaResponse Save -> retry "save feature is not implimented yet"
           Position c -> let result = boardSet b c (head players)
                         in case result of
                             Left Occupied -> retry "Spaced is already occupied."
                             Left OutOfBounds -> retry "Position is off the board."
                             Left Ko -> retry "Invalid move due to ko rule."
                             Left Suicide -> retry "Move is suicidal."
                             Right (numRemoved, b) -> let captured = case head players of
                                                            White -> (numRemoved+blackCaptured, whiteCaptured, moveNum+1)
                                                            Black -> (blackCaptured, numRemoved+whiteCaptured, moveNum+1)
                                                      in loop (tail players) b (Just c) captured ""
