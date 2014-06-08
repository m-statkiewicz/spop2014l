module State where

-- **************** data types *******************

data PieceType = Wolf | Sheep deriving Eq
	instance Show PieceType where
		show Wolf  = "W"
		show Sheep = "S"

-- przyjmujemy zwykły układ współrzędnych - (0,0) jest w lewym-dolnym rogu, pola numerowane od 0
type Pos = (Int, Int)

-- zamiana na układ współrzędnych gdzie (0,0) jest w lewym-górnym rogu, na potrzeby wyświetlania
	normalizePosition:: Pos->Pos
		normalizePosition (x, y) = (7-x, y) 

type Piece = (PieceType,Pos)

	getPosition::Piece->Pos
		getPosition (_,pos) = pos

	getType::Piece->PieceType
		getType (t,_) = t

type State = [Piece]

	toString::State->String
		toString [] = []
		toString (x:xs) = show x ++ " | " ++ toString xs

	toString'::[State]->String
		toString' [] = []
		toString' (x:xs) = toString x ++ " ;; " ++ toString' xs

	toText::State->String
		toText [] = []
		toText (x:xs) = show x ++ "\n" ++ toText xs

-- **************** some states *******************

initialState::State
	initialState = [(Wolf,(0,0)),(Sheep,(7,1)),(Sheep,(7,3)),(Sheep,(7,5)),(Sheep,(7,7))]

onePieceState::State
	onePieceState = [(Wolf,(0,0))]

testState::State
	testState = [(Wolf,(1,5)),(Sheep,(2,2)),(Sheep,(2,4)),(Sheep,(4,2)),(Sheep,(4,4))]
