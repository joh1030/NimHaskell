-- Nim game versus random player

import System.Random

type SticksPerRow = [Int]

showBoard :: SticksPerRow -> String
showBoard sticksPerRow = row1 ++ row2 ++ row3
	where
		row1 = "\n" ++ "Row 1: " ++ replicate (sticksPerRow!!0) 'x' ++ "\n"
		row2 = "Row 2: " ++ replicate (sticksPerRow!!1) 'x' ++ "\n" 
		row3 = "Row 3: " ++ replicate (sticksPerRow!!2) 'x'

validRow :: Int ->  Bool
validRow row 
	| row == 1 || row == 2 || row == 3 = True
	| otherwise = False
	
validSticks :: Int -> Int -> SticksPerRow -> Bool
validSticks row sticks sticksPerRow 
	| sticks == 0 = False -- need to check for 0 stick
	| row == 1 && (sticksPerRow !! 0) >= sticks = True
	| row == 1 &&(sticksPerRow !! 0) < sticks = False
	| row == 2 && (sticksPerRow !! 1) >= sticks = True
	| row == 2 && (sticksPerRow !! 1) < sticks = False
	| row == 3 && (sticksPerRow !! 2) >= sticks = True
	| row == 3 && (sticksPerRow !! 2) < sticks = False
	| otherwise = False

randomPlayerMove :: SticksPerRow -> StdGen -> SticksPerRow
randomPlayerMove sticksPerRow g = do
	let (row, newGen) = randomR (0,2) g
	if (sticksPerRow!!row > 0)
		then do
			let sticks = fst(randomR(1,sticksPerRow!!row) g) :: Int
			let tempRow = sticksPerRow!!row - sticks
			if (row == 0)
				then
					[tempRow, sticksPerRow!!1, sticksPerRow!!2]
			else if (row == 1)
				then
					[sticksPerRow!!0, tempRow, sticksPerRow!!2]
			else
				[sticksPerRow!!0, sticksPerRow!!1, tempRow]
	else do
		randomPlayerMove sticksPerRow newGen

checkGameOver :: SticksPerRow -> Bool
checkGameOver sticksPerRow 
	| (sticksPerRow!!0 == 0) && (sticksPerRow!!1 == 0) && (sticksPerRow!!2 == 0) = True
	| otherwise = False

changePlayer player 
	| player == "human" = "computer"
	| otherwise = "human"

play :: SticksPerRow -> String -> IO ()
play sticksPerRow player = do
	putStrLn ( showBoard sticksPerRow )
	-- Human Player
	if (player == "human")
		then do
		putStrLn "Enter a row to take sticks from"
		row <- getLine
		if (validRow (read row) == False)
			then do 
				putStrLn("\nInvalid row!\n")
				play sticksPerRow player
		else
			return()
		putStrLn "Enter number of sticks to take from that row"
		sticks <- getLine

		if (validSticks (read row) (read sticks) sticksPerRow == False)
			then do 
				putStrLn("\nInvalid number of sticks!\n")
				play sticksPerRow player
			else
				return()

		if (read row == 1)
			then do 
				let tempRow = sticksPerRow!!0 - (read sticks)
				let newSticksPerRow = [tempRow, sticksPerRow!!1, sticksPerRow!!2]
				if (checkGameOver newSticksPerRow == False)
					then do
						play newSticksPerRow (changePlayer player)
				else do
						putStrLn ( showBoard newSticksPerRow )
						putStrLn "\nHuman Player Wins!!!"
		else if (read row == 2)
			then do
				let tempRow = sticksPerRow!!1 - (read sticks)
				let newSticksPerRow = [sticksPerRow!!0, tempRow, sticksPerRow!!2]
				if (checkGameOver newSticksPerRow == False)
					then do
						play newSticksPerRow (changePlayer player)
				else do
						putStrLn ( showBoard newSticksPerRow )
						putStrLn "\nHuman Player Wins!!!"
		else if (read row == 3)
			then do
				let tempRow = sticksPerRow!!2 - (read sticks)
				let newSticksPerRow = [sticksPerRow!!0, sticksPerRow!!1, tempRow]
				if (checkGameOver newSticksPerRow == False)
					then do
						play newSticksPerRow (changePlayer player)
				else do	
						putStrLn ( showBoard newSticksPerRow )
						putStrLn "\nHuman Player Wins!!!"
		else
			return()
	-- Computer Player
	else do
		g <- newStdGen
		let sprAfterCompPlayerMove = (randomPlayerMove sticksPerRow g)
		if (checkGameOver sprAfterCompPlayerMove == False)
			then do
				play sprAfterCompPlayerMove (changePlayer player)
		else do
			putStrLn ( showBoard sprAfterCompPlayerMove )
			putStrLn "\nComputer Player Wins!!!"
	
main = do
	putStrLn "\nWecome To NIM!!!"
	play [4,3,7] "human"

	