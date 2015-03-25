module Main where

import qualified System.Console.ANSI as A

import Board
import Move
import Logic

playerSeq :: [Player]
playerSeq = [Black, White] ++ playerSeq

boardSet :: Board -> Coord -> Player -> MoveResult
boardSet board@(Board b) coord@(Coord (x,y)) p =
    if isCoordOnBoard coord board  then
        if boardGet board coord /= Empty then
            Left Occupied
        else
            logic board coord p
    else Left OutOfBounds

newline :: IO ()
newline = putStr "\n"

data GameState = GS { player :: Player,
                      board :: Board,
                      lastPlayed :: Maybe Coord,
                      moveNumber :: Int, whiteCaptured :: Int,
                      blackCaptured :: Int,
                      message :: String
                    }

startingGame :: GameState
startingGame = GS { player = Black,
                    board = initialBoard Nothing,
                    lastPlayed = Nothing,
                    moveNumber = 1,
                    whiteCaptured = 0,
                    blackCaptured = 0,
                    message = ""
                  }

main :: IO ()
main = do
    let boardSize = 19
    A.clearScreen
    A.setCursorPosition 0 0
    putStrLn $ " board size: " ++ show boardSize ++ "x" ++ show boardSize
    newline
    loop $ startingGame {board = initialBoard (Just boardSize)}
  where loop :: GameState -> IO ()
        loop gs = do
            let retry m = do
                    A.clearScreen
                    A.setCursorPosition 0 0
                    loop $ gs {message = m}
                nextPlayer = if player gs == Black then White else Black
                playerName = if player gs == Black then "Black" else "White"
            putStrLn $ " Black (X) Captured: " ++ show (blackCaptured gs)
            putStrLn $ " White (O) Captured: " ++ show (whiteCaptured gs)
            newline
            putStrLn $ " Message: " ++ message gs ++ "\n"
            print $ board gs
            newline
            move <- getMove (player gs, moveNumber gs)
            newline
            case move of
                Invalid -> retry "Invalid move."
                MetaResponse Pass -> loop $ gs { player=nextPlayer,
                                                 moveNumber=moveNumber gs + 1,
                                                 message=playerName ++ " passed"
                                               }
                MetaResponse Exit -> putStrLn "Thanks for playing!"
                MetaResponse Save -> retry "save feature is not implimented yet"
                Position c -> do
                    let result = boardSet (board gs) c (player gs)
                    case result of
                        Left Occupied         -> retry "Spaced is already occupied."
                        Left OutOfBounds      -> retry "Position is off the board."
                        Left Ko               -> retry "Invalid move due to ko rule."
                        Left Suicide          -> retry "Move is suicidal."
                        Right (numRemoved, b) ->
                            let capturedState = case player gs of
                                                    White -> gs {blackCaptured = numRemoved + blackCaptured gs}
                                                    Black -> gs {whiteCaptured = numRemoved + whiteCaptured gs}
                            in do
                                A.clearScreen
                                A.setCursorPosition 0 0
                                loop $ capturedState {moveNumber = moveNumber gs + 1,
                                                      board = b,
                                                      player=nextPlayer,
                                                      message = ""}
