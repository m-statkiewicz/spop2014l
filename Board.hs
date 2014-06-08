module Board where
import Data.Char
import State
import Utils

-- **************** data types *******************
data SqColor = Black | White

type Square = (Maybe PieceType,SqColor)  

type Board = [[Square]]

setState::Board->State->Board
setState b [] = b
setState b (p:st) = 
	updateMatrix (normalizePosition (getPosition p)) (Just (getType p), Black) (setState b st)

-- **************** output functions *******************
prettyBoard::Board->String
prettyBoard  = unlines . map (concatMap prettySquare)

prettyBoardIndent::Int->Board->String
prettyBoardIndent x = ('\n':) . concatMap ((('\n':take x (repeat ' '))++) . concatMap prettySquare)

prettySquare::Square->String
prettySquare (Nothing, Black) = "#"
prettySquare (Nothing, White) = " "
prettySquare (Just a, _) = show a

-- **************** some boards *******************
initialBoard,emptyBoard::Board

initialBoard = setState emptyBoard initialState

emptyBoard =  [[(Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black)],
				[(Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White)],
				[(Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black)],
				[(Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White)],
				[(Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black)],
				[(Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White)],
				[(Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black)],
				[(Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White)]]

