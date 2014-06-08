import System.IO  
import Data.Char
import Board
import State
import Utils
import Minmax

main = game

game = gui initialState

gui :: State->IO()
gui st = if (wolfWin st || sheepWin st)
					then
						do
						putStrLn (prettyBoard(setState emptyBoard st))
						putStr "Gra zakonczona."
						if (sheepWin st)
							then
								putStrLn "Wygraly owce."
							else
								putStrLn "Zwyciezyl wilk."
				else do 
			putStrLn "(r)uch[1379], (n)owa gra, (z)apis, (o)dczyt, (w)yjscie"
			putStrLn (prettyBoard(setState emptyBoard st))
			--endGame st
			cmd <- getLine
			getCommand st cmd

getCommand :: State->[Char]->IO()
getCommand state cmd = do
	case cmd of
		'w':_ -> return ()
		'o':_ -> readState
		'z':_ -> writeState state
		'n':_ -> gui initialState
		'r':cm -> do
			if (isValid (move state 0 (getMove cm)) ) 
			then	
				gui (fst (getPlayerMove (move state 0 (getMove cm)) Sheep 5))
			else do
				putStrLn "Wykonano niepoprawny ruch"				
				gui state
		_		->	do 
				putStrLn "Wybrano niepoprawna opcje"
				gui state

getMove::[Char]->Pos
getMove [] = (8,8)
getMove (c:ch) = result where
	r1 = if c=='7' then (-1, 1) else (0,0)
	r2 = if c=='9' then ( 1, 1) else (0,0)
	r3 = if c=='1' then (-1,-1) else (0,0)
	r4 = if c=='3' then ( 1,-1) else (0,0)
	r = addPair (addPair r1 r2) (addPair r3 r4)
	result = if r==(0,0) then (8,8) else r

readState::IO()
readState = do
			putStrLn "Podaj nazwe pliku:"
			file <- getLine
		--	if (doesFileExist file) then do
		--		putStrLn ("Plik "++ file ++" nie istnieje") 
		--		gui initialState
		--	else do
			handle <- openFile file ReadMode
			contents <- hGetContents handle
			--putStrLn(readLine handle)
			putStrLn(take 9 contents)
			hClose handle

{-
readLine :: Handle -> IO String
readLine h = catch (hGetLine h) errorHandler	where
				errorHandler e = if isEOFError e
					then return ""
					else ioError e-}

writeState::State->IO()
writeState s = do
		putStrLn "Podaj nazwe pliku:"
		file <- getLine
		handle <- openFile file WriteMode
		hPutStrLn handle (toText s)
		hClose handle
		putStrLn "Zapisano stan gry"
		gui s
