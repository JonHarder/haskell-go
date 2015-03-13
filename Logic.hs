module Logic where

import Board
import Move

import qualified Data.Set as Set
import Data.List (nub)

-- given a particular board, returns the list of positions of all stones
-- connected to stone given at point of the same color
left,right,up,down :: Coord -> Coord
left  (Coord (x,y)) = Coord (x-1,y)
right (Coord (x,y)) = Coord (x+1,y)
up    (Coord (x,y)) = Coord (x,y-1)
down  (Coord (x,y)) = Coord (x,y+1)

adjacent :: Board -> Coord -> [Coord]
adjacent b c = filter (flip isCoordOnBoard b) [left c, right c, up c, down c]

getGroup :: Board -> Coord -> [Coord]
getGroup b c = if boardGet b c == Empty then []
               else go b c (boardGet b c) Set.empty
  where go :: Board -> Coord -> Point -> Set.Set Coord -> [Coord]
        go board coord point connected =
          let touching = adjacent board coord :: [Coord]
              surrounding = Set.filter (\x -> boardGet board x == point)
                            (Set.fromList touching)
          in if Set.difference surrounding connected == Set.empty then
               coord : Set.toList connected
             else nub $ concat $ (map (\x -> go board x point (Set.union connected surrounding))
                                  (Set.toList surrounding))

numLiberties :: Board -> Coord -> Int
numLiberties b c = length $ getLiberties b c

getLiberties :: Board -> Coord -> [Coord]
getLiberties b c = let liberties coord = filter (\x -> boardGet b x == Empty) $ adjacent b coord
                       group = getGroup b c
                   in nub $ concat $ map liberties group

-- looks at group connected to coord on board, returns true
-- if group has no liberties, false if 1 or more liberties present
isDeadGroup :: Board -> Coord -> Bool
isDeadGroup b c = numLiberties b c == 0

isKo :: Board -> Bool
isKo _ = False


-- logic: after placing a stone, remove any/all adjacent dead groups.
-- after doing so, the board returns to a previous state (ko rule)
-- revert as the this move is illegal.
-- also if, after placing said stone and removing adjacent dead groups,
-- the stone and its group (if any) is dead, revert back as the move is suicidal
--
-- places stone without logic, removes adjacent stones if possible, returning
-- number of stones removed and new board
placeAndRemoveDead :: Board -> Coord -> Player -> (Int, Board)
placeAndRemoveDead board@(Board b) coord@(Coord (x,y)) p =
  let newBoard = Board $ setAt b y $ setAt (b !! y) x (Stone p) :: Board
      opponent = if p == White then Black else White
      opponentAdjacent = filter (\c -> boardGet newBoard c == (Stone opponent)) (adjacent newBoard coord)
      opponentGroups = map (getGroup newBoard) opponentAdjacent
      coords = nub $ concat opponentGroups
      deadCoords = filter (isDeadGroup newBoard) coords
  in (length deadCoords, foldl removeStone newBoard deadCoords)

logic :: Board -> Coord -> Player -> MoveResult
logic b c p = let (numRemoved, newBoard) = placeAndRemoveDead b c p
              in if isDeadGroup newBoard c then
                   Left Suicide
                 else if isKo newBoard then
                        Left Ko
                      else Right (numRemoved, newBoard)
