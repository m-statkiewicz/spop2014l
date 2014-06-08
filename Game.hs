import System.IO
import Data.Char
import Board
import State
import Utils

game = gui initialState

gui :: State->IO()
gui st = do 
			putStrLn "(r)uch[1379], (n)owa gra, (z)apis, (o)dczyt, (w)yjscie"
			putStrLn (prettyBoard(setState emptyBoard st))
			cmd <- getLine
			getCommand st cmd

getCommand :: State->[Char]->IO()
getCommand state cmd = do
	case cmd of
		'w':_ -> return ()
--		'o':_ -> readState
--		'z':_ -> writeState state
		'n':_ -> gui initialState
		'r':cm -> do
			if (isValid (move state 0 (getMove cm)) ) 
			then	
				gui (move state 0 (getMove cm))
			else 
				do
				putStrLn "Wykonano niepoprawny ruch"				
				gui state
		_		->	 putStrLn "Wybrano niepoprawna opcje"

getMove::[Char]->Pos
getMove [] = (8,8)
getMove (c:ch) = result where
	r1 = if c=='7' then ( 1,-1) else (0,0)
	r2 = if c=='9' then ( 1, 1) else (0,0)
	r3 = if c=='1' then (-1,-1) else (0,0)
	r4 = if c=='3' then (-1, 1) else (0,0)
	r = addPair (addPair r1 r2) (addPair r3 r4)
	result = if r==(0,0) then (8,8) else r

{-
readState::String->IO()
readState = do
		putStrLn "Podaj nazwe pliku:"
		file <- getLine
		handle <- openFile file ReadMode
		hClose handle
		gui testState

writeState::String->State->IO()
writeState s = do
		putStrLn "Podaj nazwe pliku:"
		file <- getLine
		handle <- openFile file WriteMode
		write handle s
		hClose handle
		gui s

write h [] = []
write h s = do
		hPutStrLn h (toText s)
		write h []
-}
