module Main where

import System.Console.ANSI
import Graphics.Vty.Widgets.All

import Board as B

main :: IO ()
main = do
  clearScreen
  setCursorPosition 0 0
  -- setSGR [SetColor Foreground Vivid Red]
  -- setSGR [SetColor Background Vivid Green]
  let board = initialBoard $ Nothing
  B.printBoard board
  -- setSGR [Reset]
  -- print $ boardDimensions initialBoard
