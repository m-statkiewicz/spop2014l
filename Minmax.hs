module Minmax where
import Data.Char
import Board
import State

getPossibleStates::State->Piece->[State]
getPossibleStates [] _ = []
getPossibleStates (x:xs) Wolf = getPossibleStates' xs x

getPossibleStates s Sheep = getPossibleStates' ([s !! 0] ++ [s !! 2] ++ [s !! 3] ++ [s !! 4]) (s !! 1) 
	++ getPossibleStates' ([s !! 0] ++ [s !! 1] ++ [s !! 3] ++ [s !! 4]) (s !! 2) ++ getPossibleStates' ([s !! 0] ++ [s !! 1] ++ [s !! 2] ++ [s !! 4]) (s !! 3)
	++ getPossibleStates' ([s !! 0] ++ [s !! 1] ++ [s !! 2] ++ [s !! 3]) (s !! 4)

getPossibleStates'::State->(Piece, Pos)->[State]
getPossibleStates' state (Wolf, pos) = result where
	x = fst pos
	y = snd pos
	r1 = if x+1<8 && y+1<8 && (isCollision (x+1, y+1) state) == False then [[(Wolf, (x+1, y+1))] ++ state] else []
	r2 = if x+1<8 && y-1>=0 && (isCollision (x+1, y-1) state) == False then [[(Wolf, (x+1, y-1))] ++ state] else []
	r3 = if x-1>=0 && y+1<8 && (isCollision (x-1, y+1) state) == False then [[(Wolf, (x-1, y+1))] ++ state] else []
	r4 = if x-1>=0 && y-1>=0 && (isCollision (x-1, y-1) state) == False then [[(Wolf, (x-1, y-1))] ++ state] else []
	result = r1 ++ r2 ++ r3 ++ r4

getPossibleStates' state (Sheep, pos) = result where
	x = fst pos
	y = snd pos
	r1 = if x+1<8 && y+1<8 && (isCollision (x+1, y+1) state) == False then [state ++ [(Sheep, (x+1, y+1))]] else []
	r2 = if x-1>=0 && y+1<8 && (isCollision (x-1, y+1) state) == False then [state ++ [(Sheep, (x-1, y+1))]] else []
	result = r1 ++ r2

isCollision::Pos->State->Bool
isCollision _ [] = False
isCollision newPos (x:xs) = result where
	pos = snd x
	result = if (fst pos) == (fst newPos) && (snd pos) == (snd newPos) then True else True && isCollision newPos xs
