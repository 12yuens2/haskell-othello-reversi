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
                     turn :: Col
                     }


initWorld = World initBoard Black


validMovesAvailable :: Board -> Col -> Bool
validMovesAvailable b c = checkAvailable b (0,0) c

checkAvailable b (x, y) c | x==(sizeOfBoard-1) && y==(sizeOfBoard-1)    = isValidMove b (x,y) c
                          | y==(sizeOfBoard-1) && isValidMove b (x,y) c = True
                          | y==(sizeOfBoard-1)                          = checkAvailable b (x+1, 0) c
                          | isValidMove b (x, y) c                      = True
                          | otherwise                                   = checkAvailable b (x,y+1) c

isValidMove :: Board -> Position -> Col -> Bool
isValidMove b (x,y) c = length (getPosList b (x,y) c) /= 0

getPosList :: Board -> Position -> Col -> [Position]
getPosList b (x,y) c = nList ++ eList ++ sList ++ wList ++ nwList ++ neList ++ swList ++ seList  
                       where
                           nList  = checkFlips []  b (x,y) (0, -1)  c
                           eList  = checkFlips []  b (x,y) (1, 0)   c
                           sList  = checkFlips []  b (x,y) (0, 1)   c
                           wList  = checkFlips []  b (x,y) (-1, 0)  c
                           nwList = checkFlips []  b (x,y) (-1, -1) c
                           neList = checkFlips []  b (x,y) (1, -1)  c
                           swList = checkFlips []  b (x,y) (-1, 1)  c
                           seList = checkFlips []  b (x,y) (1, 1)   c

-- | Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, there is a piece already there, or the move does not flip any opposing pieces)
makeMove :: Board -> Position -> Col -> Maybe Board
makeMove b (x,y) c | (containsPiece b (x,y)) = Nothing
                   | length posList == 0     = Nothing
                   | otherwise               = Just (flipping (Board (size b) (passes b) (((x,y),c):(pieces b))) posList)
                   where
                       posList = getPosList b (x,y) c

-- | Flips all the pieces in the given list of 'Position's
flipping :: Board -> [Position] -> Board
flipping b possToFlip = Board (size b) (passes b) (flipPieces (pieces b) possToFlip)

-- | Flips every piece in the pieces of the board based on the given list of positions
flipPieces 	:: [(Position, Col)] 	-- ^ The list of pieces on the board
			-> [Position] 			-- ^ The list of positions with pieces to be flipped
			-> [(Position, Col)]	-- ^ Returns a list of pieces with pieces flipped
flipPieces [] _ = []
flipPieces a [] = a
flipPieces boardPieces (newPiece:newPieces) = (flipPieces (flipPiece boardPieces newPiece) newPieces)

-- | Flips an individual piece
flipPiece :: [(Position,Col)] -> Position -> [(Position,Col)]
flipPiece [] _ = []
flipPiece (((x,y),c):pieces) (newX, newY)
	= if x == newX && y == newY
		then ((x,y),(other c)):pieces --return when a piece has been flipped
		else ((x,y),c):(flipPiece pieces (newX,newY))




-- | Checks the board or any pieces that would be flipped 
checkFlips 	:: [Position] 	-- ^ The list of positions that have pieces to be flipped
			-> Board 		-- ^ The 'Board' to check if positions contain pieces and the colours of pieces
			-> Position 	-- ^ The 'Position' where the move has been made
			-> Position 	-- ^ The position offset used to check in a specific direction (eg, (0,-1) for North, (1,-1) for NorthEast)
			-> Col 			-- ^ The 'Col' which we are flipping the pieces to
			-> [Position]	-- ^ The return type. The fuction either returns its first argument or an empty list
checkFlips returnList b (x,y) (xOffset, yOffset) c = 
	let 
		x' = x + xOffset
		y' = y + yOffset
	in 
		if containsPiece b (x',y')
			then if getPieceColor b (x',y') /= c
				then checkFlips ((x',y'):returnList) b (x',y') (xOffset,yOffset) c
				else returnList
		else []

-- | Checks that the given position of the board contains a piece
containsPiece :: Board -> Position -> Bool
containsPiece (Board s p []) (x,y) = False
containsPiece (Board s p (piece:pieces)) (x,y) 
	= if (fst (fst piece)) == x && (snd (fst piece)) == y
		then True
		else containsPiece (Board s p pieces) (x,y)


-- | Gets the colour of the piece at the given position. Assume that a piece exists in that position
getPieceColor :: Board -> Position -> Col
getPieceColor (Board s p []) (x,y) = error("No piece at that position")
getPieceColor (Board s p (piece:pieces)) (x,y)
	= if (fst (fst piece)) == x && (snd (fst piece)) == y
		then snd piece
		else getPieceColor (Board s p pieces) (x,y)



--currently unused
posZipX :: Int -> [Int] -> [Position]
posZipX x [] = []
posZipX x (y:ys) = (x,y):(posZipX x ys)

--currently unused
posZipY :: [Int] -> Int -> [Position]
posZipY [] y = []
posZipY (x:xs) y = (x,y):(posZipY xs y)



-- | Check the current score
-- Returns a pair of the number of black pieces, and the number of white pieces
checkScore :: Board -> (Int, Int)
checkScore b = (evaluate b Black, evaluate b White) 

-- | Return true if the game is complete 
-- (that is, either the board is full or there have been two consecutive passes)
gameOver :: Board -> Bool
gameOver Board {passes = 2} = True
gameOver Board {pieces = x} | (length x) < (sizeOfBoard ^ 2) = False
                            | otherwise                      = True

-- | An evaluation function for a minimax search. 
-- Given a board and a colour return an integer indicating how good the board is for that colour.
evaluate :: Board -> Col -> Int
evaluate Board {pieces = []}                _       = 0
evaluate Board {pieces = ((_, colour1):xs)} colour2  
                       | colour1 == colour2 = (evaluate (Board sizeOfBoard 0 xs) colour2) + 1
                       | otherwise          = evaluate (Board sizeOfBoard 0 xs) colour2


getPosX :: Position -> Float
getPosX (x,y) = fromIntegral(x)

getPosY :: Position -> Float
getPosY (x,y) = fromIntegral(y)
