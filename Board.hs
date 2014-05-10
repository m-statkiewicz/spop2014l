module Board where
import Data.Char
import Utils

-- **************** data types *******************

data Piece = Wolf | Sheep deriving (Eq)

data SqColor = Black | White

type Square = (Maybe Piece,SqColor)  

type Board = [[Square]]

type Pos = (Int, Int)

-- **************** output functions *******************

prettyBoard::Board->String
prettyBoard  = unlines . map (concatMap prettySquare)

prettyBoardIndent::Int->Board->String
prettyBoardIndent x = ('\n':) . concatMap ((('\n':take x (repeat ' '))++) . concatMap prettySquare)

instance Show Piece where
 show Wolf  = "W"
 show Sheep = "S"

prettySquare::Square->String
prettySquare (Nothing, Black) = "#"
prettySquare (Nothing, White) = " "
prettySquare (Just a, _) = show a

-- **************** some boards *******************

initialBoard,emptyBoard::Board
initialBoard =[[(Nothing, White), (Just Sheep, Black), (Nothing, White), (Just Sheep, Black), (Nothing, White), (Just Sheep, Black), (Nothing, White), (Just Sheep, Black)],
					[(Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White)],
					[(Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black)],
					[(Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White)],
					[(Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black)],
					[(Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White)],
					[(Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black)],
					[(Just Wolf, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White)]]

emptyBoard =  [[(Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black)],
					[(Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White)],
					[(Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black)],
					[(Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White)],
					[(Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black)],
					[(Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White)],
					[(Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black)],
					[(Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White), (Nothing, Black), (Nothing, White)]]

