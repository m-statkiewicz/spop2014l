module State where

-- **************** data types *******************

data Piece = Wolf | Sheep deriving Eq
instance Show Piece where
 show Wolf  = "W"
 show Sheep = "S"

-- przyjmujemy zwykły układ współrzędnych - (0,0) jest w lewym-dolnym rogu, pola numerowane od 0
type Pos = (Int, Int)

type State = [(Piece,Pos)]

toString::State->String
toString [] = []
toString (x:xs) = show x ++ " | " ++ toString xs

toString'::[State]->String
toString' [] = []
toString' (x:xs) = toString x ++ " ;; " ++ toString' xs

--toString [s] = show (snd s)-- (show fst(s))++";" ++(show (fst (snd s)))++";"++(show (snd (snd s)))++"\n"
--toString [s] = (show fst(s))++";" ++(show (fst (snd s)))++";"++(show (snd (snd s)))++"\n"
--toString s:st = (show fst(s))++";"++(show (fst (snd s)))++";"++(show (snd (snd s)))++"\n"++(toString st)

-- **************** some states *******************

initialState::State
initialState = [(Wolf,(0,0)),(Sheep,(7,1)),(Sheep,(7,3)),(Sheep,(7,5)),(Sheep,(7,7))]

onePieceState::State
onePieceState = [(Wolf,(0,0))]

testState::State
testState = [(Wolf,(1,5)),(Sheep,(2,2)),(Sheep,(2,4)),(Sheep,(4,2)),(Sheep,(4,4))]
