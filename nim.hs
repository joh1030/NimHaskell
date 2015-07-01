--type Board = [Char]
type SticksPerRow = [Int]

showBoard :: SticksPerRow -> String
showBoard sticksPerRow = row1 ++ row2 ++ row3
	where
		row1 = "Row 1: " ++ replicate (sticksPerRow!!0) 'x' ++ "\n"
		row2 = "Row 2: " ++ replicate (sticksPerRow!!1) 'x' ++ "\n" 
		row3 = "Row 3: " ++ replicate (sticksPerRow!!2) 'x' ++ "\n"
		
showBoardNew :: SticksPerRow -> [String]
showBoardNew sticksPerRow = map (\row -> concat ["x" | r <- [0..row-1]]) sticksPerRow

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
	
--removeSticks :: Int -> Int -> SticksPerRow -> SticksPerRow
--removeSticks row sticks sticksPerRow = sticksPerRow & element (row-1) .~ ((sticksPerRow !! (row-1)) - sticks) 

checkGameOver :: SticksPerRow -> Bool
checkGameOver sticksPerRow 
	| (sticksPerRow!!0 == 0) && (sticksPerRow!!0 == 0) && (sticksPerRow!!0 == 0) = True
	| otherwise = False

play :: SticksPerRow -> IO ()
play sticksPerRow r s = do
	putStrLn ( showBoard sticksPerRow )
	if (validRow r　== False) 
	    then do
		putStrLn "Enter a row to take sticks from"
		row <- getLine
		if (validRow (read row) == False)
			then do 
				putStrLn("\nInvalid row!\n")
				play sticksPerRow
		else
			return()
	else
	    return()
	putStrLn "Enter number of sticks to take from that row"
	sticks <- getLine

	if (validSticks (read row) (read sticks) sticksPerRow == False)
		then do 
			putStrLn("\nInvalid number of sticks!\n")
			play sticksPerRow
		else
			return()

	if (read row == 1)
		then do 
			let tempRow = sticksPerRow!!0 - (read sticks)
			let newSticksPerRow = [tempRow, sticksPerRow!!1, sticksPerRow!!2]
			play newSticksPerRow
	else if (read row == 2)
		then do
			let tempRow = sticksPerRow!!1 - (read sticks)
			let newSticksPerRow = [sticksPerRow!!0, tempRow, sticksPerRow!!2]
			play newSticksPerRow
	else if (read row == 3)
		then do
			let tempRow = sticksPerRow!!2 - (read sticks)
			let newSticksPerRow = [sticksPerRow!!0, sticksPerRow!!1, tempRow]
			play newSticksPerRow
	else
		return()
	
main = do
	putStrLn "Wecome To NIM!!!\n"
	play [4,3,7] 100 100

	