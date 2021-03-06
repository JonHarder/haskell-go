module Board where

import Data.List (intercalate)

import Move
-- import Logic

data Board = Board [[Point]] deriving Eq
data Point = Empty | Stone Player deriving Eq
data MoveError = Occupied | Ko | Suicide | OutOfBounds deriving Eq
type MoveResult = Either MoveError (Int, Board)

type Row = (Int, [Point])

instance Show Point where
  show Empty = "."
  show (Stone c) = show c

instance Show Board where
  show b = showBoard b Nothing

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
                        in " " ++ show num
                           ++ spacing num ++ showPoints points
                           ++ spacing num ++ show num

-- TODO: make showBoard inteligently add +'s based on size of board
-- instead of hard coding them
showBoard :: Board -> Maybe Coord -> String
showBoard b mLastCoord =
  let (height, width) = boardDimensions b
      charToString    = \x -> [x]
      alpha           = map charToString $ take width ['A'..'Z']
      letters         = intercalate " " alpha
  in "    " ++ letters ++ "\n" ++
     unlines (map showRow (reverse (numerate b))) ++
     "    " ++ letters

-- Board modifying/searching/logic functions

boardGet :: Board -> Coord -> Point
boardGet (Board b) (Coord (x,y)) = b !! y !! x

setAt :: [a] -> Int -> a -> [a]
setAt l index val = take index l ++ [val] ++ drop (index+1) l

isCoordOnBoard :: Coord -> Board -> Bool
isCoordOnBoard (Coord (x,y)) board = let (boardX, boardY) = boardDimensions board
                                     in x >= 0 && x < boardX
                                        && y >= 0 && y < boardY

removeStone :: Board -> Coord -> Board
removeStone (Board b) (Coord (x,y)) = Board $ setAt b y $ setAt (b !! y) x Empty

-- boardSet :: Board -> Coord -> Player -> MoveResult
-- boardSet board@(Board b) coord@(Coord (x,y)) p =
--   if isCoordOnBoard coord board  then
--     if not $ boardGet board coord == Empty then
--       Left Occupied
--     else
--       let newBoard = Board $ setAt b y $ setAt (b !! y) x (Stone p)
--       in Right newBoard -- applyLogic board coord p
--   else Left OutOfBounds

