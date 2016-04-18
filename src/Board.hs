module Board where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString.Lazy
import Data.Binary

import System.Environment
import Data.Binary
import Control.Monad

import Debug.Trace

defaultBoardSize = 8
startTime = 200000

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

-- Default board is 8x8, neither played has passed, and is empty, filled later if not reversi
initBoard = Board defaultBoardSize 0 []


-- | PlayerType represents whether the player is Human or AI (more types
-- can be added in future for different AI types)
data PlayerType = Human 
                | AI
                | Client
                | Server
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
                     gameIsOver :: Bool,
                     serverSide :: Bool,
                     sock       :: Maybe Socket
                     }
  deriving Show

-- probably don't need to encode server side info as cannot save when server running
instance Binary World where
  put (World b t sts bt wt btime wtime p v r go sd sk) = do 
                                        put b
                                        put t
                                        put sts
                                        put bt
                                        put wt
                                        put btime
                                        put wtime
                                        put p
                                        put v
                                        put r
                                        put go
                                        put sd
  get = do b <- get
           t <- get
           sts <- get
           bt <- get
           wt <- get
           btime <- get
           wtime <- get
           p <- get
           v <- get
           r <- get
           go <- get
           sd <- get
           return (World b t sts bt wt btime wtime p v r go sd Nothing)

-- Binary encoding and decoding for PlayerType
instance Binary PlayerType where
  put Human  = do put (0 :: Word8)
  put AI     = do put (1 :: Word8)
  put Server = do put (2 :: Word8)
  put Client = do put (3 :: Word8)
  get = do t <- get :: Get Word8
           case t of 
            0 -> return Human
            1 -> return AI 
            2 -> return Server
            3 -> return Client 

-- Binary encoding and decoding for colour
instance Binary Col where
  put Black = do put (0 :: Word8)
  put White = do put (1 :: Word8)
  get = do t <- get :: Get Word8
           case t of 
            0 -> return Black
            1 -> return White

-- Binary encoding and decoding for board
instance Binary Board where
  put (Board sz ps pc) = do put sz
                            put ps
                            put pc
  get = do sz <- get
           ps <- get
           pc <- get
           return (Board sz ps pc)



-- | initialises the world based on the arguments passed to it
initWorld :: [String]     -- ^ List of command line arguments
          -> IO World     -- ^ Returns initialised world
initWorld args = do w <- (setArgs args (World initBoard Black [] Human Human startTime startTime False False False False True Nothing))
                    case sock w of
                        Nothing -> return $ setBasePositions w
                        -- If client side for network get base world from server
                        -- If server side then send base world to client
                        Just s  | not (serverSide w) -> do inputByteString <- recv s 65536
                                                           let serverW = decode inputByteString
                                                           return serverW {sock = Just s, serverSide = False}
                                | otherwise          -> do let startW = setBasePositions w
                                                               outputByteString = encode startW
                                                           sendAll s outputByteString
                                                           return startW


-- | Sets 4 starting positions in world board 
setBasePositions :: World  -- ^ The world to set positions in 
                 -> World  -- ^ Returns world updated with updated positoins
setBasePositions w@(World (Board sz ps _) _ _ _ _ _ _ _ _ False _ _ _)
                 = let mid = div sz 2 in
                       w {board =  (Board sz ps 
                                    [((mid-1,mid-1), Black), ((mid-1,mid), White),
                                    ((mid,mid-1), White), ((mid,mid), Black)]
                                   )} 
setBasePositions w@(World _ _ _ _ _ _ _ _ _ True _ _ _) = w


-- | Takes a default world from initWorld and alters it depending on arguments
setArgs :: [String]  -- ^ List of command line arguments
        -> World     -- ^ The world to alter depending on flags
        -> IO World   -- ^ Returns a world updated depending on flags
setArgs [] w = return w
setArgs ("-server":xs) w = do addrInfos <- getAddrInfo
                                           (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                                           Nothing (Just "21821")
                              let serverAddr = head addrInfos
                              s <- socket (addrFamily serverAddr) Stream defaultProtocol
                              bind s (addrAddress serverAddr)
                              listen s 1
                              (conn, address) <- accept s
                              setArgs xs (w {sock = Just conn, bType = Server, wType = Client})

setArgs ("-client":xs) w = do let a = getAddrArg xs
                              addrInfos <- getAddrInfo
                                           (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                                           (Just a) (Just "21821")
                              let serverAddr = head addrInfos
                              s <- socket (addrFamily serverAddr) Stream defaultProtocol 
                              connect s (addrAddress serverAddr)
                              return w {sock = Just s, serverSide = False}

setArgs ("-s":xs) w@(World (Board _ ps pc) _ _ _ _ _ _ _ _ _ _ _ _) 
                    | xs == [] = error "A number is required after -s flag to determine size of board"
                    | readResult == [] = error "An integer is required after the -s flag"
                    | (snd (head readResult)) == "" = 
                        let val = (fst (head readResult))
                            result | val < 4   = error "Grid must be at least 4x4"
                                   | val > 16  = error "Grid cannot be larger than 16x16"
                                   | odd val   = error "Grid width must be even"
                                   | otherwise = setArgs (tail xs) w {board = (Board (fst(head readResult)) ps pc)}
                            in result
                    | otherwise = error "An integer is required after -s"
                        where readResult = (reads (head xs)) :: [(Int, String)]

setArgs ("-r":xs)  w = setArgs xs (w {chooseStart = True})
setArgs ("-ab":xs) w = if sock w == Nothing   -- make sure ai doesn't take priority over server types
                          then setArgs xs (w {bType = AI})  
                          else setArgs xs w
setArgs ("-aw":xs) w = if sock w == Nothing
                          then setArgs xs (w {wType = AI})
                          else setArgs xs w
setArgs ("-h":xs)  w = setArgs xs (w {showValid = True})
setArgs (x:xs)     _ = error ("Unrecognised flag: " ++ x)


-- | Gets a server address from arguments
getAddrArg :: [String]  -- ^ List of arguments to get address from
           -> String    -- ^ Returns the argument for host name if there is one
getAddrArg [] = error "Error, host name argument required for client to connect to"
getAddrArg xs = head xs


-- | Checks if there are any possible moves for a given colour, abstracts over looping in checkAvailable
validMovesAvailable :: Board  -- ^ The board to be checked
                    -> Col    -- ^ The colour to be checked for valid moves
                    -> Bool   -- ^ Returns False if there are no valid moves and True otherwise
validMovesAvailable b c = (length (checkNormal c b)) /= 0

checkNormal :: Col -> Board -> [Position]
checkNormal c b = checkAvailable b (0,0) c

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


-- | Gets a list of unfilled starting positions, abstracts over availableStart
-- Initial arguments
checkStart :: Board       -- ^ Board to check
           -> [Position]  -- ^ returns a list of unfilled starting positions
checkStart b = availableStart b (mid -1, mid -1)
                  where mid = div (size b) 2

-- | Loop through starting positions and get list of unfilled positions
availableStart :: Board      -- ^ Board to check
               -> Position   -- ^ position to check
               -> [Position] -- ^ returns list of unfilled starting function
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
-- (e.g. outside the range of the board, there is a piece already there, or the 
-- move does not flip any opposing pieces)
makeMove :: Board        -- ^ Board to make move on
         -> Position     -- ^ Position to place new piece
         -> Col          -- ^ Colour of new piece to place
         -> Maybe Board  -- ^ returns new board if move successful or Nothing if move was invalid
makeMove b (x,y) c | (containsPiece b (x,y)) = Nothing
                   | length posList == 0     = Nothing
                   | otherwise               = Just (flipping (Board (size b) 0 (((x,y),c):(pieces b))) posList)
                   where
                       posList = getPosList b (x,y) c

-- | similar to makeMove but for placing pieces in starting positions
startMove :: Board        -- ^ Board to place starting piece on
          -> Position     -- ^ Positon to place starting piece
          -> Col          -- ^ Colour of piece to place
          -> Maybe Board  -- ^ Returns new board if move successful or Nothing if move was invalid
startMove b (x,y) c | (containsPiece b (x,y)) = Nothing
                    | (x == mid || x == mid - 1) &&
                      (y == mid || y == mid - 1)    = Just (b {pieces = ((x,y),c):(pieces b)})
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
flipPieces boardPieces (newPiece:newPieces) = (flipPieces (flipPiece boardPieces newPiece) newPieces)

-- | Flips a single piece on the board
flipPiece :: [(Position,Col)]  -- ^ List of pieces on the board
          -> Position          -- ^ Piece to flip
          -> [(Position,Col)]  -- ^ Returns a new board with the piece flipped
flipPiece [] _ = []
flipPiece (((x,y),c):pieces) (newX, newY)
    = if x == newX && y == newY
        then ((x,y),(other c)):pieces --return when a piece has been flipped
        else ((x,y),c):(flipPiece pieces (newX,newY))


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
checkScore :: Board       -- ^ The board on which to check the current score
           -> (Int, Int)  -- ^ Returns tuple with black score and white core
checkScore b = (evaluate b Black, evaluate b White) 

-- | Return true if the game is complete 
-- (that is, either the board is full or there have been two consecutive passes)
gameOver :: Board  -- ^ Board to check for game being over
         -> Bool   -- ^ Return true of game is over and false otherwise
gameOver Board {passes = 2} = True
gameOver Board {pieces = x, size = s} | (length x) < (s ^ 2) = False
                                      | otherwise            = True

-- | An evaluation function for a minimax search. 
-- Given a board and a colour return an integer indicating how good the board is for that colour.
evaluate :: Board  -- ^ The board to evaluate
         -> Col    -- ^ The colour to evaluate the board for
         -> Int    -- ^ Returns number of pieces on boards for chosen colour
evaluate Board {pieces = []} _ = 0
evaluate Board {pieces = ((_, colour1):xs), size = s} colour2  
        | colour1 == colour2 = (evaluate (Board s 0 xs) colour2) + 1
        | otherwise          = evaluate (Board s 0 xs) colour2


-- | evaluation function used by AI which scores specific positions higher
-- depending on how valuable it is to capture that position
yusukiEvaluate :: Board  -- ^ The board to evaluate
               -> Col    -- ^ The colour to evaluate for
               -> Int    -- ^ A score depending on how good the boardstate is for chosen colour
yusukiEvaluate Board {pieces = []}                _       = 0
yusukiEvaluate Board {pieces = (((x,y), colour1):xs), size = s} colour2
      -- Give higher values for corners
        | (x,y) == (0,0) 
       || (x,y) == (0,s-1) 
       || (x,y) == (s-1,0) 
       || (x,y) == (s-1,s-1) = if colour1 == colour2 
                                  then (yusukiEvaluate (Board s 0 xs) colour2) + 1000
                                  else (yusukiEvaluate (Board s 0 xs) colour2) - 1000

       --Give lower values for positions next to corners
        | (x,y) == (1,0)    || (x,y) == (1,1)   || (x,y) == (0,1)
       || (x,y) == (0,s-2)  || (x,y) == (1,s-2) || (x,y) == (1,s-1)
       || (x,y) == (s-2,0)  || (x,y) == (s-2,1) || (x,y) == (s-1,1)
       || (x,y) == (s-1,s-2)  || (x,y) == (s-2,s-2) || (x,y) == (s-2,s-1) =
          if colour1 == colour2
            then (yusukiEvaluate (Board s 0 xs) colour2) - 400
            else (yusukiEvaluate (Board s 0 xs) colour2) + 50

        | x == 0
       || x == s-1
       || y == 0
       || y == s-1 = 
          if colour1 == colour2
            then (yusukiEvaluate (Board s 0 xs) colour2) + 100
            else (yusukiEvaluate (Board s 0 xs) colour2) - 100

      --Otherwise
        | colour1 == colour2 = (yusukiEvaluate (Board s 0 xs) colour2) + 1
        | otherwise          = yusukiEvaluate (Board s 0 xs) colour2



hirushoEvaluate :: Board -> Col -> Int
hirushoEvaluate Board {pieces = []}                _       = 0
hirushoEvaluate Board {pieces = (((x,y), colour1):xs), size = s} colour2
      -- Giver higher values for corners
        | (x,y) == (0,0) 
       || (x,y) == (0,s-1) 
       || (x,y) == (s-1,0) 
       || (x,y) == (s-1,s-1) = if colour1 == colour2 
                                  then (hirushoEvaluate (Board s 0 xs) colour2) + 1000
                                  else (hirushoEvaluate (Board s 0 xs) colour2) - 1000

        | colour1 == colour2 = (hirushoEvaluate (Board s 0 xs) colour2) + length(checkAvailable (Board s 0 (((x,y), colour1):xs)) (0,0) colour2)
        | otherwise          = hirushoEvaluate (Board s 0 xs) colour2


-- | Reverts world back to most recent move - only human player turns are 
-- recorded and reverted to
undoTurn :: World  -- ^ The world to be reverted to the previuos turn
         -> World  -- ^ returns the world in its previos turn
undoTurn w@(World _ _ [] _ _ _ _ _ _ _ _ _ _) = 
         trace ("Cannot undo further back than current state") w
undoTurn w@(World _ _ ((x,y,i,j):xs) bt wt _ _ _ _ _ _ _ _) 
         | bt == Human && wt == Human = trace "Reverted to previous player turn"
           w {board = x, turn = y, stateList = xs, bTimer = i, wTimer = j}
         | bt == Human                = trace "Reverted to previous player turn"
           w {board = x, turn = Black, stateList = xs, bTimer = i, wTimer = j}
         | wt == Human                = trace "Reverted to previous player turn"
           w {board = x, turn = White, stateList = xs, bTimer = i, wTimer = j}
         | otherwise = trace "Cannot revert during AI turn" w
