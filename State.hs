module State where
import Utils

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
-- zwraca pozycje pionka
getPosition::Piece->Pos
getPosition (t,pos) = pos
-- zwraca typ pionka
getType::Piece->PieceType
getType (t,pos) = t

type State = [Piece]
-- zamienia stan na ciąg znaków
toString::State->String
toString [] = []
toString (x:xs) = show x ++ " | " ++ toString xs

toString'::[State]->String
toString' [] = []
toString' (x:xs) = toString x ++ " ;; " ++ toString' xs

-- przygotowuje stan do zapisania do pliku
toText::State->String
toText [] = []
toText (x:xs) = show x ++ "\n" ++ toText xs

-- porusza itym pionkiem o pos 
move::State->Int->Pos->State
move [] i pos = []
move (s:sn) 0 pos = [((getType s),(addPair (getPosition s) pos))]++sn
move (s:sn) i pos = [s] ++ (move sn (i-1) pos)

--sprawdza czy pozycja jest poprawna
isPositionValid::Pos->Bool
isPositionValid (x,y) = (x>=0) && (x<=7) && (y>=0) && (y<=7)
 
--sprawdza czy pozycja jest zajeta przez inny pionek
isFree::Pos->State->Bool
isFree _ [] = True
isFree pos (s:sn) = (pos/=(getPosition s)) && isFree pos sn

--sprawdza czy stan jest poprawny
isValid::State->Bool
isValid [] = True
isValid (s:sn) = (isPositionValid (getPosition s)) && (isFree (getPosition s) sn) && (isValid sn)

-- **************** some states *******************
initialState::State
initialState = [(Wolf,(0,0)),(Sheep,(7,1)),(Sheep,(7,3)),(Sheep,(7,5)),(Sheep,(7,7))]

onePieceState::State
onePieceState = [(Wolf,(0,0))]

testState::State
testState = [(Wolf,(1,5)),(Sheep,(2,2)),(Sheep,(2,4)),(Sheep,(4,2)),(Sheep,(4,4))]
