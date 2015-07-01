-- Nim game versus smart player

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
	
checkGameOver :: SticksPerRow -> Bool
checkGameOver sticksPerRow 
	| (sticksPerRow!!0 == 0) && (sticksPerRow!!1 == 0) && (sticksPerRow!!2 == 0) = True
	| otherwise = False

changePlayer player 
	| player == "human" = "computer"
	| otherwise = "human"

toBinary :: Int -> [ Int ]
toBinary 0 = [ 0 ]
toBinary n = toBinary ( n `quot` 2 ) ++ [ n `rem` 2 ]

fullBinary :: [Int] -> [Int]
fullBinary binaryNumber
	| length binaryNumber < 4 = fullBinary (0:binaryNumber)
	| length binaryNumber == 4 = binaryNumber
	| otherwise = []

numberOfOnes :: [Int] -> Int
numberOfOnes [] = 0
numberOfOnes (x:xs)
	| x == 1 = 1 + (numberOfOnes xs)
	| otherwise	= numberOfOnes xs
	
checkModTwo :: Int -> Bool
checkModTwo x
	| (x `mod` 2) == 0 = True
	| otherwise = False
	
addRowsOfOnes :: [[Int]] -> [[Int]]
addRowsOfOnes ([]:_) = []
addRowsOfOnes x = (map head x) : addRowsOfOnes (map tail x)

ensureBinaryListIsFull :: [[Int]] -> [[Int]]
ensureBinaryListIsFull binaryList = map (\x -> fullBinary x) binaryList

	
makeParityList :: [Int] -> [Int]
makeParityList numberOfOnes = map (\x -> if checkModTwo x == True then 0 else 1 ) numberOfOnes 
	
checkKernelState :: SticksPerRow -> Bool
checkKernelState sticksPerRow = do
	let binarySticksPerRow = ensureBinaryListIsFull (map (toBinary) sticksPerRow) --convert to binary
	let rearrangeBinary = addRowsOfOnes binarySticksPerRow
	let parity = map (\x -> numberOfOnes x) rearrangeBinary --count # of ones in each binary number
	let ones = makeParityList parity
	if (elem 1 ones) 
		then False			
	else True
	
checkRow :: SticksPerRow -> Int -> SticksPerRow
checkRow sticksPerRow row
	| (sticksPerRow !! (row-1)) == 0 = sticksPerRow 
	| (sticksPerRow !! (row-1)) > 0 && (checkKernelState sticksPerRow) = sticksPerRow
	| (sticksPerRow !! (row-1)) > 0 && (row == 1) = checkRow [((sticksPerRow !! 0) - 1), sticksPerRow !! 1, sticksPerRow !! 2] row
	| (sticksPerRow !! (row-1)) > 0 && (row == 2) = checkRow [sticksPerRow !! 0, ((sticksPerRow !! 1) - 1), sticksPerRow !! 2] row
	| (sticksPerRow !! (row-1)) > 0 && (row == 3) = checkRow [sticksPerRow !! 0, sticksPerRow !! 1, ((sticksPerRow !! 2) - 1)] row
	| otherwise = sticksPerRow

computerPlayerSmart :: SticksPerRow -> SticksPerRow
computerPlayerSmart sticksPerRow = do
	let tempBoard = checkRow sticksPerRow 1
	if (checkKernelState tempBoard) 
		then tempBoard
	else if (checkKernelState (checkRow sticksPerRow 2))
		then checkRow sticksPerRow 2
	else if (checkKernelState (checkRow sticksPerRow 3))
		then checkRow sticksPerRow 3
	else sticksPerRow

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
		let sprAfterCompPlayerMove = computerPlayerSmart sticksPerRow
		if (checkGameOver sprAfterCompPlayerMove == False)
			then do
				play sprAfterCompPlayerMove (changePlayer player)
		else do
			putStrLn ( showBoard sprAfterCompPlayerMove )
			putStrLn "\nComputer Player Wins!!!"
	
main = do
	putStrLn "\nWecome To NIM!!!"
	play [4,3,7] "human"

	