import System.IO
import Data.Char
import Board
import State

game = gui initialState

--gui :: State->IO()
gui a = do 
			putStrLn "(r)uch[1379], (n)owa gra, (z)apis, (o)czyt, (w)yjscie"
			putStrLn (prettyBoard emptyBoard)
			cmd <- getLine
			gui (getCommand a cmd)

--getCommand :: State->[Char]->State
getCommand state cmd = do
	case cmd of
		'w':_ -> []
		'o':_ -> readState
		'z':_ -> writeState state
		'n':_ -> do
					gui initialState
--		'r':x -> gui move state x

readState::String->IO()
readState = do
		putStrLn "Podaj nazwe pliku:"
		file <- getLine
		handle <- openFile file ReadMode
		hClose handle
		gui testState

--writeState::String->State->IO()
writeState s = do
		putStrLn "Podaj nazwe pliku:"
		file <- getLine
		handle <- openFile file WriteMode
		write handle s
		hClose handle
		gui s

write h [] = []
write h s = do
		hPutStrLn h "Write"
		write h []

