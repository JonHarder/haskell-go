module Main where

import qualified System.Console.ANSI as A
-- import Graphics.Vty.Widgets.All

import Board
import Move
import Control.Monad (when)

data Turn = One | Two deriving Show

playerSeq = [Black, White] ++ playerSeq

main :: IO ()
main = do
  -- setSGR [SetColor Foreground Vivid Red]
  -- setSGR [SetColor Background Vivid Green]
  let board = initialBoard $ Nothing
  loop playerSeq board ""
 where loop players b message = do
         A.clearScreen
         A.setCursorPosition 0 0
         when (not (null message)) $ putStrLn message
         print b
         move <- getMove $ head players
         case move of
          MetaResponse Pass -> loop (tail players) b (show (head players) ++ " passed")
          MetaResponse Exit -> putStrLn "Bye!"
          MetaResponse Save -> loop players b "Not implimented yet"
          Coord _ -> let newBoard = boardSet b move (head players)
                     in if newBoard == b then
                          loop players b "Position is off the board."
                        else
                          loop (tail players) newBoard ""
