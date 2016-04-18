module Draw where

import Graphics.Gloss
import Board
import Datatype
import Data.Tuple

import Debug.Trace

data Bitmaps  -- | Bitmaps are the data structure that stores all the bitmap files at
              -- the start of the game so they can be accessed by other draw functions
  = Bitmaps {
    bp    :: Picture,
    wp    :: Picture,
    hp    :: Picture,
    tp    :: Picture,
    gob   :: Picture,
    gow   :: Picture,
    gobt  :: Picture,
    gowt  :: Picture,
    draw  :: Picture,
    pause :: Picture
    }

-- | Load images at the start of the game
initPictures :: IO Bitmaps
initPictures = do
  bp    <- loadBMP "res/black2.bmp"
  wp    <- loadBMP "res/white2.bmp"
  hp    <- loadBMP "res/hint2.bmp"
  tp    <- loadBMP "res/blank.bmp" 
  gob   <- loadBMP "res/goblack.bmp"
  gow   <- loadBMP "res/gowhite.bmp"
  gobt  <- loadBMP "res/gobt.bmp"
  gowt  <- loadBMP "res/gowt.bmp"
  draw  <- loadBMP "res/draw.bmp"
  pause <- loadBMP "res/pause.bmp"
  return $ Bitmaps bp wp hp tp gob gow gobt gowt draw pause

width = 800.0
height = 800.0
gridPos = width/2.0

defaultSquareWidth = 8


{- Helper functons for defining grid spaces -}

-- | Calculates the factor the bitmaps have to be scaled 
-- depending on the board size
scaleFactor :: Int  -- ^ Size of the board
            -> Float
scaleFactor sz = defaultSquareWidth / fromIntegral sz

-- | Calculates the width of a single square based on board sz
rectSize :: Int    -- ^ The sz of the board/number of squares in a row
         -> Float  -- ^ returns the width of a single box
rectSize sz = width / fromIntegral sz

-- | Calculates the radius of a piece based on the board sz
pieceSize :: Int    -- ^ The sz of the board/number of squares in a row
          -> Float  -- ^ returns the radius of a piece
pieceSize sz = rectSize sz / 2.0

-- | Translate between our custom 'Col' and the 'Color' needed for drawing
getCol :: Col -> Color
getCol Black = black
getCol White = white

-- | Converts an x position to an x position on the GUI for squares
squarePosX :: Int   -- ^ Size of the grid
           -> Float -- ^ The position to be converted
           -> Float -- ^ Returns the converted position
squarePosX s x = -gridPos + pieceSize s + (rectSize s * x) 

-- | Converts an y position to an y position on the GUI for squares
squarePosY :: Int   -- ^ Size of the grid
           -> Float -- ^ The position to be converted 
           -> Float -- ^ Returns the converted position
squarePosY s y = gridPos - pieceSize s - (rectSize s * y) 


hintFunc :: Bool -> Col -> (Board -> [Position])
hintFunc True _ = checkStart
hintFunc _    c = checkAvailable c


{- Drawing -}

-- | Main draw world function passed into playIO
drawWorldIO :: Bitmaps -- ^ Data structure with loaded bitmaps stored
            -> World   -- ^ The world to be drawn
            -> IO Picture
drawWorldIO bitmaps w@(World b@(Board sz _ pc) turn _ bt wt btime wtime isP h r go _ _)
  -- Draw gameover
  | go        = do let screen =  (getWinner bitmaps b btime wtime)
                   totals <- (currentPieces b)
                   return $ pictures [screen, totals]

  -- Draw pause
  | isP       = return $ pause bitmaps

  -- Draw hints
  | h         = do hints <- drawHints (hp bitmaps) sz (hintFunc r turn b)
                   world <- drawWorldIO bitmaps w{ showValid = False}
                   return $ pictures [world, hints]

  -- Draw otherwise
  | otherwise = do board  <- drawBoardBMP (tp bitmaps) sz
                   pieces <- drawPiecesBMP (bp bitmaps) (wp bitmaps) sz pc
                   totals <- (currentPieces b)
                   timeb  <- timeLeft bt btime (1700,(-500))
                   timew  <- timeLeft wt wtime ((-2300),(-500))
                   return $ pictures [board, pieces, totals, timeb, timew]


-- | Draw the time left for each player
timeLeft :: PlayerType     -- ^ Only draw if the playertype is human
         -> Int            -- ^ The time left
         -> (Float, Float) -- ^ The (x,y) offset where the time should be drawn on the screen
         -> IO Picture
timeLeft Human time (x,y) = drawText ("Time: " ++ show (div time 100)) x y
timeLeft _     _    _     = return Blank


-- | Draw current number of pieces for each player
currentPieces :: Board       -- ^ Board to draw pieces for
              -> IO Picture
currentPieces b = do
    numb <- drawText ("Black: " ++ (show $ evaluate b Black)) 1700 1000
    numw <- drawText ("White: " ++ (show $ evaluate b White)) (-2300) 1000
    return $ pictures [numb, numw]


{- Drawing for individual components -}

-- | Draws the empty board
drawBoardBMP :: Picture  -- ^ Picture of the board tile
             -> Int      -- ^ The size of the board
             -> IO Picture
drawBoardBMP tp sz = do
  emptyPieces <- mapM (drawEmptyPiece tp sz) [(x,y) | x <- [0..(sz-1)], y <- [0..(sz-1)]]
  return $ pictures emptyPieces


-- | Gets a string stating the outcome of a game based on a board
getWinner :: Bitmaps -- ^ The saved bitmaps
          -> Board   -- ^ The board to evaluate to find winner
          -> Int     -- ^ The current time left for black
          -> Int     -- ^ The current time left for white
          -> Picture -- Returns the winning screen
getWinner bitmaps b btime wtime | btime <= 0 = gowt bitmaps
                                | wtime <= 0 = gobt bitmaps
                                | x == y     = draw bitmaps
                                | x > y      = gob bitmaps
                                | otherwise  = gow bitmaps
  where (x,y) = checkScore b


-- | Draws text
drawText :: String   -- ^ String to draw
         -> Float    -- ^ x coordinate to draw at
         -> Float    -- ^ y coordinate to draw at
         -> IO Picture
drawText text x y = return $ scale 0.25 0.25 $ translate x y $ Text text


-- | Draw hint pc on the board
drawHints :: Picture     -- ^ Picture for the individual hint pieces
          -> Int         -- ^ The size of the board on which hints are being drawn
          -> [Position]  -- ^ The list of positions to put hint pc
          -> IO Picture
drawHints _ _ [] = return Blank
drawHints hp sz ((x,y):ps) = do
  hints <- drawHints hp sz ps
  return $ pictures [ translate (squarePosX sz $ fromIntegral x) (squarePosY sz $ fromIntegral y) $ scale (scaleFactor sz) (scaleFactor sz) hp
                    , hints
                    ]


-- | Draw an empty space onto the board
drawEmptyPiece :: Picture   -- ^ Picture for the individual spaces
               -> Int       -- ^ The size of the board
               -> Position  -- ^ The position draw the empty space on the board
               -> IO Picture
drawEmptyPiece tp sz (x,y) = 
  return $ translate (squarePosX sz $ fromIntegral x ) (squarePosY sz $ fromIntegral y) $ scale (scaleFactor sz) (scaleFactor sz) tp


-- | Draws pc onto the boards
drawPiecesBMP :: Picture            -- ^ Picture for the black piece
              -> Picture            -- ^ Picture for the white piece
              -> Int                -- ^ The size of the board 
              -> [(Position, Col)]  -- ^ List of positions and colours for drawing pc
              -> IO Picture
drawPiecesBMP _ _ _ [] = return Blank
drawPiecesBMP bp wp sz (((x,y),col):ps) = do
  piece   <- drawPieceBMP bp wp sz (squarePosX sz $ fromIntegral x) (squarePosY sz $ fromIntegral y) col
  pieces  <- drawPiecesBMP bp wp sz ps
  return $ pictures [piece, pieces]


-- | Draw a single piece onto the board
drawPieceBMP :: Picture  -- ^ Picture for the black piece
             -> Picture  -- ^ Picture for the white piece
             -> Int      -- ^ The size of the board
             -> Float    -- ^ x position to draw piece
             -> Float    -- ^ y position to draw piece
             -> Col      -- ^ colour of piece that is to be drawn
             -> IO Picture
drawPieceBMP bp _ sz x y Black = 
  return $ translate x y $ scale (scaleFactor sz) (scaleFactor sz) bp
drawPieceBMP _ wp sz x y White =
  return $ translate x y $ scale (scaleFactor sz) (scaleFactor sz) wp
