module Board where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString.Lazy
import Data.Binary
import Data.Maybe

import System.Environment
import Control.Monad

import Debug.Trace

import Datatype

-- The default board width and starting time in milliseconds
defaultBoardSize = 8
startTime = 60000

-- | Changes to the other colour
other :: Col -> Col
other Black = White
other White = Black

-- Default board is 8x8, neither played has passed, and is empty, filled later if not reversi
initBoard = Board defaultBoardSize 0 []

-- | initialises the world based on the arguments passed to it
initWorld :: [String]     -- ^ List of command line arguments
          -> IO World     -- ^ Returns initialised world
initWorld args = do w <- setArgs args (World initBoard Black [] Human Human startTime startTime False False False False True Nothing)
                    case sock w of
                        -- return world normally if nor on network
                        Nothing -> return $ setBasePositions w
                                -- If client side for network get base world from server
                        Just s  | not (serverSide w) -> do inputByteString <- recv s 65536
                                                           let serverW = decode inputByteString
                                                           return serverW {sock = Just s, serverSide = False}
                                -- If server side then send base world to client
                                | otherwise          -> do let startW = setBasePositions w
                                                               outputByteString = encode startW
                                                           sendAll s outputByteString
                                                           return startW


-- | Sets 4 starting positions in world board 
setBasePositions :: World  -- ^ The world to set positions in 
                 -> World  -- ^ Returns world updated with updated positoins
setBasePositions w@(World (Board sz ps _) _ _ _ _ _ _ _ _ False _ _ _)
                 = let mid = div sz 2 in
                       w {board =  Board sz ps 
                                    [((mid-1,mid-1), Black), ((mid-1,mid), White),
                                    ((mid,mid-1), White), ((mid,mid), Black)]
                                   } 
setBasePositions w@(World _ _ _ _ _ _ _ _ _ True _ _ _) = w


-- | Takes a default world from initWorld and alters it depending on arguments
setArgs :: [String]  -- ^ List of command line arguments
        -> World     -- ^ The world to alter depending on flags
        -> IO World  -- ^ Returns a world updated depending on flags
setArgs [] w = return w

-- For server argument set up server socket and set player types to network
setArgs ("-server":xs) w = do addrInfos <- getAddrInfo
                                           (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                                           Nothing (Just "21821")
                              let serverAddr = head addrInfos
                              s <- socket (addrFamily serverAddr) Stream defaultProtocol
                              bind s (addrAddress serverAddr)
                              listen s 1
                              (conn, address) <- accept s
                              setArgs xs (w {sock = Just conn, bType = Network, wType = Network})

-- For client argument get following hostname argument and connect to host
setArgs ("-client":xs) w | null xs = error "Error, host name argument required for client to connect to"
                         | otherwise = do 
                              let a = (head xs)
                              addrInfos <- getAddrInfo
                                           (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                                           (Just a) (Just "21821")
                              let serverAddr = head addrInfos
                              s <- socket (addrFamily serverAddr) Stream defaultProtocol 
                              connect s (addrAddress serverAddr)
                              return w {sock = Just s, serverSide = False}

-- For size argument get following integer size argument and set board size
setArgs ("-s":xs) w
                     | null xs = error "A number is required after -s flag to determine size of board"
                     | null readResult = error "An integer is required after the -s flag"
                     | snd (head readResult) == "" 
                    && newSize > 3
                    && newSize <17
                    && even newSize = setArgs (tail xs) (w {board = ((board w) {size = newSize})})
                     | snd (head readResult) == "" = error "Size must be at least 4, no more than 16 and even"
                     | otherwise = error "An integer is required after -s"
                        where readResult = reads (head xs) :: [(Int, String)]
                              newSize    = (fst (head readResult))

-- For r argument set chooseStart to true so players can choose start positions
setArgs ("-r":xs)  w = setArgs xs (w {chooseStart = True})

-- For h arguments set showValid to true so hints are on
setArgs ("-h":xs)  w = setArgs xs (w {showValid = True})

-- Otherwise if their is an AI argument, ignore it if server is on and set AI if server is off
-- If argument is not recognised then display an error
setArgs (x:xs)     w | isJust (sock w) 
                    && (x == "-ab" || x == "-rb" || x == "-aw" || x == "-rw") = setArgs xs w
                     | x == "-ab" = setArgs xs (w {bType = AI})
                     | x == "-rb" = setArgs xs (w {bType = Random})
                     | x == "-aw" = setArgs xs (w {wType = AI})
                     | x == "-rw" = setArgs xs (w {wType = Random})
                     | otherwise  = error ("Unrecognised flag: " ++ x)


-- | Abstracts over checkAvailable, gets a list of all available moves on a board
checkNormal :: Col         -- ^ Colour to check available moves for
            -> Board       -- ^ Board to check
            -> [Position]  -- ^ Returns list of available moves
checkNormal c b = checkAvailable b (0,0) c


-- | Loops through board checking if there are any valid moves
checkAvailable :: Board      -- ^ The board to be checked
               -> Position   -- ^ The position to be checked
               -> Col        -- ^ The colour for checking whether this position would be a valid move
               -> [Position] -- ^ Returns a list of all valid positions
-- If a position is at the end of a row, checkAvailable for start of next row, 
-- if at end of last row finish checking, otherwise just checkAvailable for next in row
checkAvailable b (x, y) c | x==(size b - 1) 
                         && y==(size b - 1) 
                         && isValidMove b (x,y) c = [(x,y)]
                          | x==(size b - 1) 
                         && y==(size b - 1)       = []
                          | y==(size b - 1) 
                         && isValidMove b (x,y) c = (x,y):checkAvailable b (x+1,0) c
                          | y==(size b - 1)       = checkAvailable b (x+1, 0) c
                          | isValidMove b (x,y) c = (x,y):checkAvailable b (x,y+1) c
                          | otherwise             = checkAvailable b (x,y+1) c


-- | Gets a list of unfilled starting positions
checkStart :: Board       -- ^ Board to check
           -> [Position]  -- ^ returns a list of unfilled starting positions
checkStart b = filter (\x -> not (containsPiece b x)) [(mid -1, mid -1),(mid -1, mid),(mid, mid -1),(mid, mid)]
                  where mid = div (size b) 2


-- | Checks if a move is valid (it will actually flip some pieces)
isValidMove :: Board  -- ^ The board to be checked 
         -> Position  -- ^ The position the move will place a piece 
         -> Col       -- ^ The colour of the piece being placed
         -> Bool      -- ^ Returns True if move is valid and False otherwise
isValidMove b (x,y) c = not (containsPiece b (x,y)) && not (null (getPosList b (x,y) c))

-- | Gets a list of positions of pieces that will be flipped if a piece of the 
--   specified colour is places in the specified position
getPosList :: Board      -- ^ The board to check
           -> Position   -- ^ The position the piece is to be placed
           -> Col        -- ^ The colour of the piece being played
           -> [Position] -- ^ Returns a list of pieces that will be flipped
getPosList b (x,y) c = nList ++ eList ++ sList ++ wList ++ nwList ++ neList ++ swList ++ seList  
                       where
                           -- list of flips for all directions
                           nList  = checkFlips []  b (x,y) (0, -1)  c
                           eList  = checkFlips []  b (x,y) (1, 0)   c
                           sList  = checkFlips []  b (x,y) (0, 1)   c
                           wList  = checkFlips []  b (x,y) (-1, 0)  c
                           nwList = checkFlips []  b (x,y) (-1, -1) c
                           neList = checkFlips []  b (x,y) (1, -1)  c
                           swList = checkFlips []  b (x,y) (-1, 1)  c
                           seList = checkFlips []  b (x,y) (1, 1)   c

-- | Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, there is a piece already there, or the 
-- move does not flip any opposing pieces)
makeMove :: Board        -- ^ Board to make move on
         -> Position     -- ^ Position to place new piece
         -> Col          -- ^ Colour of new piece to place
         -> Maybe Board  -- ^ returns new board if move successful or Nothing if move was invalid
makeMove b (x,y) c | containsPiece b (x,y)   = Nothing
                   | null posList            = Nothing
                   | otherwise               = Just (flipping (Board (size b) 0 (((x,y),c):pieces b)) posList)
                   where
                       posList = getPosList b (x,y) c

-- | similar to makeMove but for placing pieces in starting positions
startMove :: Board        -- ^ Board to place starting piece on
          -> Position     -- ^ Positon to place starting piece
          -> Col          -- ^ Colour of piece to place
          -> Maybe Board  -- ^ Returns new board if move successful or Nothing if move was invalid
startMove b (x,y) c | containsPiece b (x,y) = Nothing
                    | (x == mid || x == mid - 1) &&
                      (y == mid || y == mid - 1)    = Just (b {pieces = ((x,y),c):pieces b})
                    | otherwise                     = Nothing
                        where mid = div (size b) 2


-- | Flips all the pieces in the given list of 'Position's
flipping :: Board       -- ^ Board upon which to flip pieces
         -> [Position]  -- ^ List of positions to flip
         -> Board       -- ^ Returns new board with pieces flipped
flipping b posToFlip = Board (size b) (passes b) (flipPieces (pieces b) posToFlip)


-- | Flips pieces on the board based on the given list of positions
flipPieces  :: [(Position, Col)]  -- ^ The list of pieces on the board
            -> [Position]         -- ^ The list of positions with pieces to be flipped
            -> [(Position, Col)]  -- ^ Returns a list of pieces with pieces flipped
flipPieces [] _ = []
flipPieces a [] = a
flipPieces boardPieces (newPiece:newPieces) = flipPieces (flipPiece boardPieces newPiece) newPieces


-- | Flips a single piece on the board
flipPiece :: [(Position,Col)]  -- ^ List of pieces on the board
          -> Position          -- ^ Piece to flip
          -> [(Position,Col)]  -- ^ Returns a new board with the piece flipped
flipPiece [] _ = []
flipPiece (((x,y),c):pieces) (newX, newY)
    = if x == newX && y == newY
        then ((x,y),other c):pieces --return when a piece has been flipped
        else ((x,y),c):flipPiece pieces (newX,newY)


-- | Checks the board for any pieces that would be flipped 
checkFlips :: [Position]  -- ^ The list of positions that have pieces to be flipped
           -> Board       -- ^ The 'Board' to check if positions contain pieces and the colours of pieces
           -> Position    -- ^ The 'Position' where the move has been made
           -> Position    -- ^ The position offset used to check in a specific 
                          --   direction (eg, (0,-1) for North, (1,-1) for NorthEast)
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
containsPiece :: Board     -- ^ Board to check
              -> Position  -- ^ Positon to check for containing a piece
              -> Bool      -- ^ Returns true if position contains a piece and false otherwise
containsPiece (Board s p []) (x,y) = False
containsPiece (Board s p (piece:pieces)) (x,y) 
    = (fst (fst piece) == x && snd (fst piece) == y) || containsPiece (Board s p pieces) (x,y)


-- | Gets the colour of the piece at the given position. Assume that a piece 
-- exists in that position
getPieceColor :: Board     -- ^ The board to check
              -> Position  -- ^ The position on the board to check
              -> Col       -- ^ Returns the colour of the piece at this spot
getPieceColor (Board s p []) (x,y) = error "No piece at that position"
getPieceColor (Board s p (piece:pieces)) (x,y)
    = if fst (fst piece) == x && snd (fst piece) == y
        then snd piece
        else getPieceColor (Board s p pieces) (x,y)


-- | Check the current score
-- Returns a pair of the number of black pieces, and the number of white pieces
checkScore :: Board       -- ^ The board on which to check the current score
           -> (Int, Int)  -- ^ Returns tuple with black score and white core
checkScore b = (evaluate b Black, evaluate b White) 

-- | Return true if the game is complete 
-- (that is, either the board is full or there have been two consecutive passes)
gameOver :: Board  -- ^ Board to check for game being over
         -> Bool   -- ^ Return true of game is over and false otherwise
gameOver Board {passes = 2} = True
gameOver Board {pieces = x, size = s} | length x < (s ^ 2) = False
                                      | otherwise           = True

-- | An evaluation function for a minimax search. 
-- Given a board and a colour return an integer indicating how good the board is for that colour.
evaluate :: Board  -- ^ The board to evaluate
         -> Col    -- ^ The colour to evaluate the board for
         -> Int    -- ^ Returns number of pieces on boards for chosen colour
evaluate Board {pieces = []} _ = 0
evaluate Board {pieces = ((_, colour1):xs), size = s} colour2  
        | colour1 == colour2 = evaluate (Board s 0 xs) colour2 + 1
        | otherwise          = evaluate (Board s 0 xs) colour2


-- | Reverts world back to most recent move - only human player turns are 
-- recorded and reverted to
undoTurn :: World  -- ^ The world to be reverted to the previuos turn
         -> World  -- ^ returns the world in its previos turn

-- Do not undo if there are no previous states
undoTurn w@(World _ _ [] _ _ _ _ _ _ _ _ _ _) = 
         trace "Cannot undo further back than current state" w

-- Revert to previous human player turn otherwise
undoTurn w@(World _ _ ((x,y,i,j):xs) _ ciw_wt _ _ _ _ _ _ _ _) = 
         trace "Reverted to previous player turn"
         w {board = x, turn = y, stateList = xs, bTimer = i, wTimer = j}
