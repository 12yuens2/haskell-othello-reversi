module Board where

import System.Environment

import Debug.Trace

defaultBoardSize = 8
startTime = 20000

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
initBoard = Board defaultBoardSize 0 []
-- | PlayerType represents whether the player is Human or AI (more types
-- can be added in future for different AI types)
data PlayerType = Human | AI
  deriving (Show, Eq)

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).
data World = World { board :: Board,
                     turn :: Col,
                     stateList :: [(Board, Col, Int, Int)], -- Need to store colour of turn in case of pass
                                                              -- (Int,Int) for storing timers
                     bType :: PlayerType,
                     wType :: PlayerType,
                     bTimer :: Int,
                     wTimer :: Int,
                     pause :: Bool,
                     showValid :: Bool,
                     chooseStart :: Bool,
                     gameIsOver :: Bool
                     }



-- | initialises the world based on the arguments passed to it
initWorld :: [String]  -- ^ List of command line arguments
          -> World     -- ^ Returns initialised world
initWorld args = setBasePositions (setArgs args (World initBoard Black [] Human Human startTime startTime False False False False))

-- | Sets 4 starting positions in world boars 
setBasePositions :: World  -- ^ The world to set positions in 
                 -> World  -- ^ Returns world updated with updated positoins
setBasePositions (World (Board sz ps _) t sts bt wt btime wtime p v False go)
                 = let mid = div sz 2 in
                       World (Board sz ps 
                             [((mid-1,mid-1), Black), ((mid-1,mid), White),
                             ((mid,mid-1), White), ((mid,mid), Black)]) 
                             t sts bt wt btime wtime p v False go
setBasePositions (World b t sts bt wt btime wtime p v True go) = World b t sts bt wt btime wtime p v True go


-- | Takes a default world from initWorld and alters it depending on arguments
setArgs :: [String]  -- ^ List of command line arguments
        -> World     -- ^ The world to alter depending on flags
        -> World     -- ^ Returns a world updated depending on flags
setArgs [] w = w
setArgs ("-s":xs) (World (Board _ ps pc) t sts bt wt btime wtime p v r go) 
                    | xs == [] = error "A number is required after -s flag to determine size of board"
                    | readResult == [] = error "An integer is required after the -s flag"
                    | (snd (head readResult)) == "" = 
                        let val = (fst (head readResult))
                            result | val < 4   = error "Grid must be at least 4x4"
                                   | val > 16  = error "Grid cannot be larger than 16x16"
                                   | odd val   = error "Grid width must be even"
                                   | otherwise = setArgs (tail xs) (World (Board (fst(head readResult)) ps pc) t sts bt wt btime wtime p v r go)
                            in result
                    | otherwise = error "An integer is required after -s"
                        where readResult = (reads (head xs)) :: [(Int, String)]
setArgs ("-r":xs)  w = setArgs xs (w {chooseStart = True})
setArgs ("-ab":xs) w = setArgs xs (w {bType = AI})
setArgs ("-aw":xs) w = setArgs xs (w {wType = AI})
setArgs ("-v":xs)  w = setArgs xs (w {showValid = True})
setArgs (x:xs)     _ = error ("Unrecognised flag: " ++ x)

-- | Checks if there are any possible moves for a given colour, abstracts over looping in checkAvailable
validMovesAvailable :: Board  -- ^ The board to be checked
                    -> Col    -- ^ The colour to be checked for valid moves
                    -> Bool   -- ^ Returns False if there are no valid moves and True otherwise
validMovesAvailable b c = (length (checkAvailable b (0,0) c)) /= 0

-- | Loops through board checking if there are any valid moves
checkAvailable :: Board      -- ^ The board to be checked
               -> Position   -- ^ The position to be checked
               -> Col        -- ^ The colour for checking whether this position would be a valid move
               -> [Position] -- ^ Returns True if a valid move is found and False otherwise
checkAvailable b (x, y) c | x==(size b - 1) 
                         && y==(size b - 1) 
                         && isValidMove b (x,y) c = [(x,y)]
                          | x==(size b - 1) 
                         && y==(size b - 1)       = []
                          | y==(size b - 1) 
                         && isValidMove b (x,y) c = ((x,y):(checkAvailable b (x+1,0) c))
                          | y==(size b - 1)       = checkAvailable b (x+1, 0) c
                          | isValidMove b (x,y) c = ((x,y):(checkAvailable b (x,y+1) c))
                          | otherwise             = checkAvailable b (x,y+1) c

checkStart :: Board -> [Position]
checkStart b = availableStart b (mid -1, mid -1)
                  where mid = div (size b) 2

availableStart :: Board -> Position -> [Position]
availableStart b (x, y) | x == (mid - 1) 
                       && y == (mid - 1) 
                       && containsPiece b (x,y) = availableStart b (mid,mid - 1)
                        | x == (mid - 1) 
                       && y == (mid - 1)        = (x,y):(availableStart b (mid,mid - 1))
                        | x ==  mid 
                       && y == (mid - 1) 
                       && containsPiece b (x,y) = availableStart b (mid - 1,mid)
                        | x ==  mid 
                       && y == (mid - 1)        = (x,y):(availableStart b (mid - 1,mid))
                        | x == (mid -1) 
                       && containsPiece b (x,y) = availableStart b (mid,mid)
                        | x == (mid - 1)        = (x,y):(availableStart b (mid,mid))
                        | containsPiece b (x,y) = []
                        | otherwise             = [(x,y)]
                            where mid = div (size b) 2


-- | Checks if a move is valid (it will actually flip some pieces)
isValidMove :: Board  -- ^ The board to be checked 
         -> Position  -- ^ The position the move will place a piece 
         -> Col       -- ^ The colour of the piece being placed
         -> Bool      -- ^ Returns True if move is valid and False otherwise
isValidMove b (x,y) c = (not (containsPiece b (x,y))) && ((length (getPosList b (x,y) c)) /= 0)

-- | Gets a list of positions of pieces that will be flipped if a piece of the 
--   specified colour is places in the specified position
getPosList :: Board      -- ^ The board to check
           -> Position   -- ^ The position the piece is to be placed
           -> Col        -- ^ The colour of the piece being played
           -> [Position] -- ^ Returns a list of pieces that will be flipped
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

startMove :: Board -> Position -> Col -> Maybe Board
startMove b (x,y) c | (containsPiece b (x,y)) = Nothing
                    | (x == mid || x == mid - 1) &&
                      (y == mid || y == mid - 1)    = Just (b {pieces = ((x,y),c):(pieces b)})
                    | otherwise                     = Nothing
                        where mid = div (size b) 2

startState :: [(Position, Col)] -> Bool
startState xs = (length xs) < 4 

-- | Flips all the pieces in the given list of 'Position's
flipping :: Board -> [Position] -> Board
flipping b possToFlip = Board (size b) (passes b) (flipPieces (pieces b) possToFlip)

-- | Flips every piece in the pieces of the board based on the given list of positions
flipPieces  :: [(Position, Col)]  -- ^ The list of pieces on the board
            -> [Position]         -- ^ The list of positions with pieces to be flipped
            -> [(Position, Col)]  -- ^ Returns a list of pieces with pieces flipped
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




-- | Checks the board for any pieces that would be flipped 
checkFlips :: [Position]  -- ^ The list of positions that have pieces to be flipped
           -> Board       -- ^ The 'Board' to check if positions contain pieces and the colours of pieces
           -> Position    -- ^ The 'Position' where the move has been made
           -> Position    -- ^ The position offset used to check in a specific direction (eg, (0,-1) for North, (1,-1) for NorthEast)
           -> Col         -- ^ The 'Col' which we are flipping the pieces to
           -> [Position]  -- ^ The return type. The fuction either returns its first argument or an empty list
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


-- | Gets the colour of the piece at the given position. Assume that a piece 
-- exists in that position
getPieceColor :: Board     -- ^ The board to check
              -> Position  -- ^ The position on the board to check
              -> Col       -- ^ Returns the colour of the piece at this spot
getPieceColor (Board s p []) (x,y) = error("No piece at that position")
getPieceColor (Board s p (piece:pieces)) (x,y)
    = if (fst (fst piece)) == x && (snd (fst piece)) == y
        then snd piece
        else getPieceColor (Board s p pieces) (x,y)


-- | Check the current score
-- Returns a pair of the number of black pieces, and the number of white pieces
checkScore :: Board -> (Int, Int)
checkScore b = (evaluate b Black, evaluate b White) 

-- | Return true if the game is complete 
-- (that is, either the board is full or there have been two consecutive passes)
gameOver :: Board -> Bool
gameOver Board {passes = 2} = True
gameOver Board {pieces = x, size = s} | (length x) < (s ^ 2) = False
                                      | otherwise            = True

-- | An evaluation function for a minimax search. 
-- Given a board and a colour return an integer indicating how good the board is for that colour.
evaluate :: Board -> Col -> Int
evaluate Board {pieces = []}                _       = 0
evaluate Board {pieces = ((_, colour1):xs), size = s} colour2  
        | colour1 == colour2 = (evaluate (Board s 0 xs) colour2) + 1
        | otherwise          = evaluate (Board s 0 xs) colour2


--better score for corners
evaluate2 :: Board -> Col -> Int
evaluate2 Board {pieces = []}                _       = 0
evaluate2 Board {pieces = (((x,y), colour1):xs), size = s} colour2  
        | (x,y) == (0,0) || (x,y) == (0,s-1) || (x,y) == (s-1,0) || (x,y) == (s-1,s-1) 
           = if colour1 == colour2 
                then (evaluate2 (Board s 0 xs) colour2) + 1000
                else (evaluate2 (Board s 0 xs) colour2) - 1000
        | colour1 == colour2 = (evaluate2 (Board s 0 xs) colour2) + 1
        | otherwise          = evaluate2 (Board s 0 xs) colour2


getPosX :: Position -> Float
getPosX (x,y) = fromIntegral(x)

getPosY :: Position -> Float
getPosY (x,y) = fromIntegral(y)


-- | Reverts world back to most recent move - only human player turns are 
-- recorded and reverted to
undoTurn :: World  -- ^ The world to be reverted to the previos turn
         -> World  -- ^ returns the world in its previos turn
undoTurn (World b c [] bt wt btime wtime p v r go) = 
         trace ("Cannot undo further back than current state") 
               (World b c [] bt wt btime wtime p v r go)
undoTurn (World b c ((x,y,i,j):xs) Human Human btime wtime p v r go)  = 
         trace "Reverted to previous player turn" 
               (World x y xs Human Human i j p v r go)
undoTurn (World b Black ((x,y,i,j):xs) Human wt btime wtime p v r go) = 
         trace "Reverted to previous player turn" 
               (World x Black xs Human wt i j p v r go)
undoTurn (World b White ((x,y,i,j):xs) bt Human btime wtime p v r go) = 
         trace "Reverted to previous player turn" 
               (World x White xs bt Human i j p v r go)
undoTurn w = trace "Cannot revert during AI turn" w

