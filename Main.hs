module Main where

import qualified System.Console.ANSI as A
-- import Graphics.Vty.Widgets.All

import Board
import Move

data Turn = One | Two deriving Show

playerSeq = [Black, White] ++ playerSeq

main :: IO ()
main = do
  -- setSGR [SetColor Foreground Vivid Red]
  -- setSGR [SetColor Background Vivid Green]
  let board = initialBoard $ Nothing
  loop playerSeq board
 where loop players b = do
         A.clearScreen
         A.setCursorPosition 0 0
         print b
         move <- getMove $ head players
         let newBoard = boardSet b move (head players)
         loop (tail players) newBoard
