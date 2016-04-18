module Draw where

import Graphics.Gloss
import Board
import Datatype
import Data.Tuple

import Debug.Trace


data Bitmaps = Bitmaps {
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

initPictures :: IO Bitmaps
initPictures = do
  bp  <- loadBMP "res/black2.bmp"
  wp  <- loadBMP "res/white2.bmp"
  hp  <- loadBMP "res/hint2.bmp"
  tp  <- loadBMP "res/blank.bmp" 
  gob <- loadBMP "res/goblack.bmp"
  gow <- loadBMP "res/gowhite.bmp"
  gobt <- loadBMP "res/gobt.bmp"
  gowt <- loadBMP "res/gowt.bmp"
  draw <- loadBMP "res/draw.bmp"
  pause <- loadBMP "res/pause.bmp"
  return $ Bitmaps bp wp hp tp gob gow gobt gowt draw pause

width = 800.0
height = 800.0
gridPos = width/2.0

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

{- Drawing with Bitmap images -}


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

defaultSquareWidth = 8

scaleFactor :: Int -> Float
scaleFactor sz = defaultSquareWidth / fromIntegral sz


hintFunc :: Bool -> Col -> (Board -> [Position])
hintFunc True _ = checkStart
hintFunc _    c = checkNormal c

--IO version of draw world
drawWorldIO :: Bitmaps -> World -> IO Picture
-- Draw gameover
drawWorldIO bitmaps (World b _ _ _ _ btime wtime _ _ _ True _ _) = return $ getWinner bitmaps b btime wtime

-- Draw pause
drawWorldIO bitmaps (World _ _ _ _ _ _ _ True _ _ _ _ _) = return $ pause bitmaps

-- Draws hints for reversi start
drawWorldIO bitmaps w@(World b turn _ _ _ _ _ _ True r _ _ _) = do
  hints <- drawHints (hp bitmaps) (size b) (hintFunc r turn b)
  world <- drawWorldIO bitmaps w{ showValid = False}
  return $ pictures [world, hints]

-- Draw otherwise
drawWorldIO bitmaps (World (Board sz ps pc) _ _ bt wt btime wtime _ _ _ _ _ _) = do
  board     <- drawBoardBMP (tp bitmaps) sz
  pieces    <- drawPiecesBMP (bp bitmaps) (wp bitmaps) sz pc
  numBlack  <- drawText ("Black: " ++ (show $ evaluate (Board sz ps pc) Black)) 1700 1000
  numWhite  <- drawText ("White: " ++ (show $ evaluate (Board sz ps pc) White)) (-2300) 1000
  timeBlack <- blackTime bt btime
  timeWhite <- whiteTime wt wtime
  return $ pictures [ board, pieces
                    , numBlack, numWhite
                    ,timeBlack, timeWhite]


timeLeft :: PlayerType -> Int -> (Int, Int) -> IO Picture
timeLeft Human time (x,y) = drawText ("Time: " ++ show (div time 100)) x y
timeLeft _     _    _     = return Blank

blackTime :: PlayerType -> Int -> IO Picture
blackTime Human btime = drawText ("Time: " ++ show (div btime 100)) 1700 (-500)
blackTime _     _     = return Blank

whiteTime :: PlayerType -> Int -> IO Picture
whiteTime Human wtime = drawText ("Time: " ++ show (div wtime 100)) (-2300) (-500)
whiteTime _     _     = return Blank

{- Draw for individual components -}

-- | Draws the empty board
drawBoardBMP :: Picture  -- ^ Picture of the board tile
             -> Int      -- ^ The size of the board
             -> IO Picture
drawBoardBMP tp sz = do
  emptyPieces <- mapM (drawEmptyPiece tp sz) [(x,y) | x <- [0..(sz-1)], y <- [0..(sz-1)]]
  return $ pictures emptyPieces


-- | Gets a string stating the outcome of a game based on a board
getWinner :: Bitmaps
          -> Board   -- ^ The board to evaluate to find winner
          -> Int     -- ^ The current time left for black
          -> Int     -- ^ The current time left for white
          -> Picture  -- Returns the winning screen
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
