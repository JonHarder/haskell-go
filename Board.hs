module Board where

import Data.List (intercalate)

data Board = Board [[Point]]

type Row = (Int, [Point])

data Point = Empty | Stone Color deriving Eq

data Color = White | Black deriving Eq

instance Show Point where
  show Empty = "."
  show (Stone s) = case s of
    White -> "O"
    Black -> "X"

boardDimensions :: Board -> (Int, Int)
boardDimensions (Board b) = (length b, length (head b))

initialBoard :: Maybe Int -> Board
initialBoard mSize = let size = maybe 19 id mSize
                     in Board $ replicate size $ replicate size Empty

numerate :: Board -> [(Int, [Point])]
numerate (Board b) = zip [1..] b

plusRowStr :: [Point] -> String
plusRowStr points = let numberPoints = zip [1..] points
                        plusify (num,point) = if num `elem` [4,10,16] && point == Empty then
                                                "+"
                                              else
                                                show point
                    in unwords $ map plusify numberPoints


showRow :: Row -> String
showRow (num, points) = let rowStr ps = unwords (map show ps)
                            spacing n = if n < 10 then "  " else " "
                            showPoints points = if num `elem` [4,10,16] then
                                                  plusRowStr points
                                                else
                                                  rowStr points
                        in show num ++ spacing num ++ showPoints points ++ spacing num ++ show num

printBoard :: Board -> IO ()
printBoard b = do
  let (height, width) = boardDimensions b
      charToString = \x -> [x]
      alpha = map charToString $ take width ['A'..'Z']
      letters = intercalate " " alpha
  putStrLn $ "   " ++ letters
  mapM_ (putStrLn . showRow) $ reverse (numerate b)
  putStrLn $ "   " ++ letters
