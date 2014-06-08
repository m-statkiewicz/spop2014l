module Minmax where
import Data.Char
import Board
import State

-- zwarca ruch dla gracza
getPlayerMove::State->PieceType->Int->(State, Int)
getPlayerMove state pieceType depth = result where
	result = getBestState (getMinmax (getPossibleStates state pieceType) pieceType depth)
	
getMinmax::[State]->PieceType->Int->[(State, Int)]
getMinmax [] _ _ = []
getMinmax (x:xs) pieceType depth = (x, minmax x Wolf depth pieceType) : getMinmax xs pieceType depth
	
getBestState::[(State, Int)]->(State, Int)
getBestState [x] = x
getBestState (x:y:ys) 	| (snd x) > (snd y)	= getBestState(x:ys)
						| otherwise 		= getBestState(y:ys)

minmax::State->PieceType->Int->PieceType->Int
minmax state pieceType k maximizingPlayer = if (isWolfWin state || isSheepsWin state) then getEndGameScore state pieceType else 
		if k == 0 then getUtility state pieceType else if maximizingPlayer /= pieceType then getMax (countMinmax (getPossibleStates state pieceType) pieceType k maximizingPlayer) else 
			getMin (countMinmax (getPossibleStates state pieceType) pieceType k maximizingPlayer)

getMax::[(State, Int)]->Int
getMax [] = 0
getMax [x] = snd x
getMax (x:y:ys) | (snd x) > (snd y)	= getMax (x:ys)
				| otherwise 		= getMax (y:ys)

getMin::[(State, Int)]->Int
getMin [] = 0
getMin [x] = snd x
getMin (x:y:ys) | (snd x) < (snd y)	= getMin (x:ys)
				| otherwise 		= getMin (y:ys)

countMinmax::[State]->PieceType->Int->PieceType->[(State, Int)]
countMinmax [] _ _ _ = [] 
countMinmax (x:xs) pieceType k maximizingPlayer = result where
	player = if pieceType == Wolf then Sheep else Wolf
	result = (x, minmax x player (k-1) maximizingPlayer) : countMinmax xs pieceType k maximizingPlayer


getUtility::State->PieceType->Int
getUtility (x:xs) Sheep = result where
	w = snd(x)
	p1 = snd(xs !! 0)
	p2 = snd(xs !! 1)
	p3 = snd(xs !! 2)
	p4 = snd(xs !! 3)
	d0 = fst p1
	d1 = sqrt (fromIntegral((fst p1 - fst p2)^2+(snd p1 - snd p2)^2))
	d2 = sqrt (fromIntegral((fst p2 - fst p3)^2+(snd p2 - snd p3)^2))
	d3 = sqrt (fromIntegral((fst p3 - fst p4)^2+(snd p3 - snd p4)^2))
	d4 = 8 - fst p4
	
	-- kara za dziury blisko wilka
	c0 = if d0 > 2 then sqrt (fromIntegral((fst p1 - fst w)^2+(snd p1 - snd w)^2)) else 0.0
	c1 = if d1 > 2 then sqrt (fromIntegral((fst p2 - fst w)^2+(snd p2 - snd w)^2)) else 0.0
	c2 = if d2 > 2 then sqrt (fromIntegral((fst p3 - fst w)^2+(snd p3 - snd w)^2)) else 0.0
	c3 = if d3 > 2 then sqrt (fromIntegral((fst p4 - fst w)^2+(snd p4 - snd w)^2)) else 0.0
	c4 = if d4 > 2 then sqrt (fromIntegral((fst p4 - fst w)^2+(snd p4 - snd w)^2)) else 0.0
	
	y1 = snd(snd (xs !! 0))
	y2 = snd(snd (xs !! 1))
	y3 = snd(snd (xs !! 2))
	y4 = snd(snd (xs !! 3))
	result = 340 - 8*((y1-y2)^2 + (y2-y3)^2 + (y3-y4)^2) - 5*(floor(d1 + d2 + d3) + d0 + d4) - floor(10*(c0 + c1 + c2 + c3 + c4))
	
getUtility (x:xs) Wolf = 10*snd(snd x)

-- zwraca wartosc funkcji oceny dla wygranej
getEndGameScore::State->PieceType->Int
getEndGameScore state Sheep = if isSheepsWin state then 300 else -300

getEndGameScore state Wolf = if isSheepsWin state then -300 else 300 

-- czy wilk wygral
isWolfWin::State->Bool
isWolfWin (x:xs) = if snd(snd x) == 0 then True else False

-- czy owce wygraly
isSheepsWin::State->Bool
isSheepsWin s = if length (getPossibleStates s Wolf) == 0 then True else False


-- zwraca wszystkie mozliwe ruchy dla Wolf lub Sheeps
getPossibleStates::State->PieceType->[State]
getPossibleStates [] _ = []

getPossibleStates (x:xs) Wolf = getPossibleStates' xs x

getPossibleStates s Sheep = getPossibleStates' ([s !! 0] ++ [s !! 2] ++ [s !! 3] ++ [s !! 4]) (s !! 1) 
	++ getPossibleStates' ([s !! 0] ++ [s !! 1] ++ [s !! 3] ++ [s !! 4]) (s !! 2) ++ getPossibleStates' ([s !! 0] ++ [s !! 1] ++ [s !! 2] ++ [s !! 4]) (s !! 3)
	++ getPossibleStates' ([s !! 0] ++ [s !! 1] ++ [s !! 2] ++ [s !! 3]) (s !! 4)

getPossibleStates'::State->(PieceType, Pos)->[State]
getPossibleStates' state (Wolf, pos) = result where
	x = fst pos
	y = snd pos
	r1 = if x+1<8 && y+1<8 && (isCollision (addPair (x,y) (1,1)) state) == False then [[(Wolf, (addPair (x,y) (1,1)))] ++ state] else []
	r2 = if x+1<8 && y-1>=0 && (isCollision (addPair (x,y) (1,-1)) state) == False then [[(Wolf, (addPair (x,y) (1,-1)))] ++ state] else []
	r3 = if x-1>=0 && y+1<8 && (isCollision (addPair (x,y) (-1,1)) state) == False then [[(Wolf, (addPair (x,y) (-1,1)))] ++ state] else []
	r4 = if x-1>=0 && y-1>=0 && (isCollision (addPair (x,y) (-1,-1)) state) == False then [[(Wolf, (addPair (x,y) (-1,-1)))] ++ state] else []
	result = r1 ++ r2 ++ r3 ++ r4

getPossibleStates' state (Sheep, pos) = result where
	x = fst pos
	y = snd pos
	r1 = if x+1<8 && y-1>=0 && (isCollision (addPair (x,y) (1,-1)) state) == False then [state ++ [(Sheep, (addPair (x,y) (1,-1)))]] else []
	r2 = if x-1>=0 && y-1>=0 && (isCollision (addPair (x,y) (-1,-1)) state) == False then [state ++ [(Sheep, (addPair (x,y) (-1,-1)))]] else []
	result = r1 ++ r2

-- sprawdza czy pionek ruszajac sie nie skoliduje sie z innym
isCollision::Pos->State->Bool
isCollision _ [] = False
isCollision newPos (x:xs) = result where
	pos = snd x
	result = if (fst pos) == (fst newPos) && (snd pos) == (snd newPos) then True else True && isCollision newPos xs
