module Board where

import Debug.Trace

sizeOfBoard = 8

data Col = Black | White
  deriving (Show, Eq)

other :: Col -> Col
other Black = White
other White = Black

type Position = (Int, Int)

-- A Board is a record containing the board size (a board is a square grid, n *
-- n), the number of consecutive passes, and a list of pairs of position and
-- the colour at that position.

data Board = Board { size :: Int,
                     passes :: Int,
                     pieces :: [(Position, Col)]
                   }
  deriving Show

-- Default board is 8x8, neither played has passed, with 4 initial pieces 
initBoard = Board sizeOfBoard 0 [((3,3), Black), ((3,4), White),
                       ((4,3), White), ((4,4), Black)]

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).
data World = World { board :: Board,
                     turn :: Col }

initWorld = World initBoard Black

-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, there is a piece already there,
-- or the move does not flip any opposing pieces)
makeMove :: Board -> Position -> Col -> Maybe Board
makeMove b (x,y) c = if (containsPiece b (x,y)) 
	then Nothing
	else 
		let posList = ((checkFlipsN [] b (x,y) c) ++ (checkFlipsE [] b (x,y) c) ++ (checkFlipsS [] b (x,y) c) ++ (checkFlipsW [] b (x,y) c) ++ (checkFlipsNW [] b (x,y) c) ++ (checkFlipsNE [] b (x,y) c) ++ (checkFlipsSW [] b (x,y) c) ++ (checkFlipsSE [] b (x,y) c)  )
		in if length(posList) == 0 
			then Nothing
			else Just (flipping (Board (size b) ((passes b)+1) (((x,y),c):(pieces b))) posList)
	--let 
	--	nList = checkFlipsN b (x,y) c 
	--	eList = checkFlipsE b (x,y) c
	--	sList = checkFlipsS b (x,y) c
	--	wList = checkFlipsW b (x,y) c
	--	posList = nList ++ eList ++ sList ++ wList
	--in Just (flipping (Board (size b) ((passes b)+1) (((x,y),c):(pieces b))) posList)

flipping :: Board -> [Position] -> Board
flipping b possToFlip = Board (size b) (passes b) (flipPieces (pieces b) possToFlip)

flipPieces :: [(Position, Col)] -> [Position] -> [(Position, Col)]
flipPieces [] _ = []
flipPieces a [] = a
flipPieces boardPieces (newPiece:newPieces) = (flipPieces (flipPiece boardPieces newPiece) newPieces)


flipPiece :: [(Position,Col)] -> Position -> [(Position,Col)]
flipPiece [] _ = []
flipPiece (((x,y),c):pieces) (newX, newY)
	= if x == newX && y == newY
		then trace("flipped piece at " ++ show (x,y)) $ ((x,y),(other c)):pieces
		else trace("didnt flip piece at " ++ show (x,y)) $ ((x,y),c):(flipPiece pieces (newX,newY))




-- Checks for flipping north of the piece
-- only y changes negatively

--need to refactor the spaghetti 

checkFlipsN :: [Position] -> Board -> Position -> Col -> [Position]
checkFlipsN returnList b (x,y) col = if containsPiece b (x,y-1) 
	then if getPieceColor b (x,y-1) /= col
		then trace ("found piece north to flip at" ++ show (x,y-1)) $ (checkFlipsN ((x,y-1):returnList) b (x,y-1) col)
		else trace ("found piece north at " ++ show (x,y-1)) $ returnList
	else trace ("no pieces north") []

checkFlipsE :: [Position] -> Board -> Position -> Col -> [Position]
checkFlipsE returnList b (x,y) col = if containsPiece b (x+1,y) 
	then if getPieceColor b (x+1,y) /= col
		then trace ("found piece north to flip at" ++ show (x+1,y)) $ (checkFlipsE ((x+1,y):returnList) b (x+1,y) col)
		else trace ("found piece north at " ++ show (x+1,y)) $ returnList
	else trace ("no pieces north") []

checkFlipsS :: [Position] -> Board -> Position -> Col -> [Position]
checkFlipsS returnList b (x,y) col = if containsPiece b (x,y+1) 
	then if getPieceColor b (x,y+1) /= col
		then trace ("found piece north to flip at" ++ show (x,y+1)) $ (checkFlipsS ((x,y+1):returnList) b (x,y+1) col)
		else trace ("found piece north at " ++ show (x,y+1)) $ returnList
	else trace ("no pieces north") []

checkFlipsW :: [Position] -> Board -> Position -> Col -> [Position]
checkFlipsW returnList b (x,y) col = if containsPiece b (x-1,y) 
	then if getPieceColor b (x-1,y) /= col
		then trace ("found piece north to flip at" ++ show (x-1,y)) $ (checkFlipsW ((x-1,y):returnList) b (x-1,y) col)
		else trace ("found piece north at " ++ show (x-1,y)) $ returnList
	else trace ("no pieces north") []

checkFlipsNW :: [Position] -> Board -> Position -> Col -> [Position]
checkFlipsNW returnList b (x,y) col = if containsPiece b (x-1,y-1) 
	then if getPieceColor b (x-1,y-1) /= col
		then trace ("found piece north to flip at" ++ show (x-1,y-1)) $ (checkFlipsNW ((x-1,y-1):returnList) b (x-1,y-1) col)
		else trace ("found piece north at " ++ show (x-1,y-1)) $ returnList
	else trace ("no pieces north") []

checkFlipsNE :: [Position] -> Board -> Position -> Col -> [Position]
checkFlipsNE returnList b (x,y) col = if containsPiece b (x+1,y-1) 
	then if getPieceColor b (x+1,y-1) /= col
		then trace ("found piece north to flip at" ++ show (x+1,y-1)) $ (checkFlipsNE ((x+1,y-1):returnList) b (x+1,y-1) col)
		else trace ("found piece north at " ++ show (x+1,y-1)) $ returnList
	else trace ("no pieces north") []

checkFlipsSW :: [Position] -> Board -> Position -> Col -> [Position]
checkFlipsSW returnList b (x,y) col = if containsPiece b (x-1,y+1) 
	then if getPieceColor b (x-1,y+1) /= col
		then trace ("found piece north to flip at" ++ show (x-1,y+1)) $ (checkFlipsSW ((x-1,y+1):returnList) b (x-1,y+1) col)
		else trace ("found piece north at " ++ show (x-1,y+1)) $ returnList
	else trace ("no pieces north") []

checkFlipsSE :: [Position] -> Board -> Position -> Col -> [Position]
checkFlipsSE returnList b (x,y) col = if containsPiece b (x+1,y+1) 
	then if getPieceColor b (x+1,y+1) /= col
		then trace ("found piece north to flip at" ++ show (x+1,y+1)) $ (checkFlipsSE ((x+1,y+1):returnList) b (x+1,y+1) col)
		else trace ("found piece north at " ++ show (x+1,y+1)) $ returnList
	else trace ("no pieces north") []


--checks that the given position of the board contains a piece
containsPiece :: Board -> Position -> Bool
containsPiece (Board s p []) (x,y) = False
containsPiece (Board s p (piece:pieces)) (x,y) 
	= if (fst (fst piece)) == x && (snd (fst piece)) == y
		then True
		else containsPiece (Board s p pieces) (x,y)


--gets the color of the piece at the give position
--assumes there actually is a piece at that position
getPieceColor :: Board -> Position -> Col
getPieceColor (Board s p []) (x,y) = error("No piece at that position")
getPieceColor (Board s p (piece:pieces)) (x,y)
	= if (fst (fst piece)) == x && (snd (fst piece)) == y
		then snd piece
		else getPieceColor (Board s p pieces) (x,y)



posZipX :: Int -> [Int] -> [Position]
posZipX x [] = []
posZipX x (y:ys) = (x,y):(posZipX x ys)

posZipY :: [Int] -> Int -> [Position]
posZipY [] y = []
posZipY (x:xs) y = (x,y):(posZipY xs y)

-- Check the current score
-- Returns a pair of the number of black pieces, and the number of
-- white pieces
checkScore :: Board -> (Int, Int)
checkScore = undefined

-- Return true if the game is complete (that is, either the board is
-- full or there have been two consecutive passes)
gameOver :: Board -> Bool
gameOver = undefined

-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> Col -> Int
evaluate = undefined


getPosX :: Position -> Float
getPosX (x,y) = fromIntegral(x)

getPosY :: Position -> Float
getPosY (x,y) = fromIntegral(y)
