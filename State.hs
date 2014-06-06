module State where

-- **************** data types *******************

data Piece = Wolf | Sheep

type Pos = (Int, Int)

type State = [(Piece,Pos)]

toString::State->String	
toString [] = []
--toString s = "A"++"B"
toString [s] = (show fst(s))++";"++(show (fst (snd s)))++";"++(show (snd (snd s)))++"\n"
--toString s:st = (show fst(s))++";"++(show (fst (snd s)))++";"++(show (snd (snd s)))++"\n"++(toString st)

-- **************** some states *******************

initialState::State
initialState = [(Wolf,(0,0)),(Sheep,(7,1)),(Sheep,(7,3)),(Sheep,(7,5)),(Sheep,(7,7))]

onePieceState::State
onePieceState = [(Wolf,(0,0))]

testState::State
testState = [(Sheep,(3,3)),(Sheep,(3,4)),(Sheep,(4,3)),(Sheep,(4,4))]
