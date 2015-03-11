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

-- infinite loop
getGroup :: Board -> Coord -> [Coord]
getGroup b c = if boardGet b c == Empty then []
               else go b c (boardGet b c) Set.empty
  where go :: Board -> Coord -> Point -> Set.Set Coord -> [Coord]
        go board coord point connected = let touching = adjacent board coord :: [Coord]
                                             -- the set of connected stones of like color to the initial stone
                                             surrounding = Set.filter (\x -> boardGet board x == point)
                                                           (Set.fromList touching)
                                         -- if all the adjacent stones have already been accounted for,
                                         -- the search is complete and we can return the found coordinates
                                         in if Set.difference surrounding connected == Set.empty then
                                              coord : Set.toList connected
                                            -- otherwise, recurse through the adjacent stones
                                            else nub $ concat $ (map (\x -> go board x point (Set.union connected surrounding))
                                                                 (Set.toList surrounding))
