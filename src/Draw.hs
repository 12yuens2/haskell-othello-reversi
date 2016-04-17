module Draw where

import Graphics.Gloss
import Board
import Data.Tuple
import System.IO.Unsafe (unsafePerformIO)

import Debug.Trace

width = 800.0
height = 800.0
gridPos = width/2.0

-- | Calculates the width of a single square based on board sz
rectSize :: Int    -- ^ The sz of the board/number of squares in a row
         -> Float  -- ^ returns the width of a single box
rectSize sz = width / (fromIntegral sz)

-- | Calculates the radius of a piece based on the board sz
pieceSize :: Int    -- ^ The sz of the board/number of squares in a row
          -> Float  -- ^ returns the radius of a piece
pieceSize sz = ((rectSize sz)/ 2.0)

-- | Translate between our custom 'Col' and the 'Color' needed for drawing
getCol :: Col -> Color
getCol Black = black
getCol White = white

{- Drawing with Bitmap images -}


-- | Converts an x position to an x position on the GUI for squares
squarePosX :: Int   -- ^ Size of the grid
           -> Float -- ^ The position to be converted
           -> Float -- ^ Returns the converted position
squarePosX s x = -gridPos + (pieceSize s) + ((rectSize s)* x) 

-- | Converts an y position to an y position on the GUI for squares
squarePosY :: Int   -- ^ Size of the grid
           -> Float -- ^ The position to be converted 
           -> Float -- ^ Returns the converted position
squarePosY s y = gridPos - (pieceSize s) - ((rectSize s)* y) 

defaultSquareWidth = 8

scaleFactor :: Int -> Float
scaleFactor sz = defaultSquareWidth / fromIntegral(sz)


-- | General fucntion to load a bitmap to an IO Picture
bmp :: FilePath -> Picture
bmp file = unsafePerformIO $ loadBMP file 
  --bitmap <- loadBMP file
  --let picture = bitmap
  --picture

-- | Loads bmp picture of black piece
blackPiece :: Picture
blackPiece = bmp "res/black2.bmp"

-- | Loads bmp picture of white piece
whitePiece :: Picture 
whitePiece = bmp "res/white2.bmp"

-- | Loads bmp picture of blank space
blankPiece :: Picture
blankPiece = bmp "res/blank.bmp"

-- | Loads bmp picture of hint piece
hintPiece :: Picture
hintPiece = bmp "res/hint2.bmp"

-- | Loads bmp picture of gameover
gameOverScreen :: Picture
gameOverScreen = bmp "res/gameover.bmp"

-- ^ Draws the world and information based on its state
--drawWorldBMP :: World    -- ^ The world which is to be drawn 
--             -> Picture

----Draw gameover
--drawWorldBMP (World (Board sz ps pieces) turn _ _ _ _ _ True ) = pictures [ gameOverScreen, (drawText (getWinner (Board sz ps pieces)) 0 0)]

---- Draws hints for reversi start
--drawWorldBMP (World (Board sz ps pieces) turn _ _ _ True True _ ) = 
--       pictures [ (drawBoardBMP sz)
--                , (drawHints sz (checkStart (Board sz ps pieces)))
--                , (drawPiecesBMP sz pieces)
--                , drawText ("Black: " ++ (show $ evaluate (Board sz ps pieces) Black)) (-2300) (-500)
--                , drawText ("White: " ++ (show $ evaluate (Board sz ps pieces) White)) (-2300) 0
--                ]

----Draw hints
--drawWorldBMP (World (Board sz ps pieces) turn _ _ _ True _ _) = 
--       pictures [ (drawBoardBMP sz)
--                , (drawHints sz (checkAvailable (Board sz ps pieces) (0,0) turn))
--                , (drawPiecesBMP sz pieces)
--                , drawText ("Black: " ++ (show $ evaluate (Board sz ps pieces) Black)) (-2300) (-500)
--                , drawText ("White: " ++ (show $ evaluate (Board sz ps pieces) White)) (-2300) 0
--                ]


---- Draw otherwise
--drawWorldBMP (World (Board sz ps pieces) _ _ _ _ _ _ _ ) = 
--    pictures     [ (drawBoardBMP sz)
--                , (drawPiecesBMP sz pieces)
--                , drawText ("Black: " ++ (show $ evaluate (Board sz ps pieces) Black)) (-2300) (-500)
--                , drawText ("White: " ++ (show $ evaluate (Board sz ps pieces) White)) (-2300) 0
--                ]

--IO version of draw world
drawWorldIO :: World -> IO Picture
-- Draw gameover
drawWorldIO (World (Board sz ps pc) turn _ _ _ btime wtime _ _ _ True _ _) = do
  text      <- drawText (getWinner (Board sz ps pc) btime wtime) 0 0
  return $ pictures [gameOverScreen, text]

-- Draw pause
drawWorldIO (World (Board sz ps pc) turn _ _ _ _ _ True _ _ _ _ _) = do
  text  <- drawText ("PAUSED") 0 0
  return $ pictures [gameOverScreen, text]

-- Draws hints for reversi start
drawWorldIO w@(World (Board sz ps pc) turn _ _ _ btime wtime _ True True _ _ _) = do
  hints <- drawHints sz (checkStart (Board sz ps pc))
  world <- drawWorldIO w{ showValid = False, chooseStart = False}
  return $ pictures [world, hints]

-- Draw hints
drawWorldIO w@(World (Board sz ps pc) turn _ _ _ btime wtime _ True _ _ _ _) = do
  hints <- drawHints sz (checkAvailable (Board sz ps pc) (0,0) turn)
  world <- drawWorldIO w{ showValid = False }
  return $ pictures [world, hints]


-- Draw otherwise
drawWorldIO (World (Board sz ps pc) _ _ _ _ btime wtime _ _ _ _ _ _) = do
  board     <- drawBoardBMP sz
  pieces    <- drawPiecesBMP sz pc
  numBlack  <- drawText ("Black: " ++ (show $ evaluate (Board sz ps pc) Black)) 1700 1000
  numWhite  <- drawText ("White: " ++ (show $ evaluate (Board sz ps pc) White)) (-2300) 1000
  timeBlack <- drawText ("Time: " ++ (show (div btime 100))) 1700 (-500)
  timeWhite <- drawText ("Time: " ++ (show (div wtime 100))) (-2300) (-500)
  return $ pictures [ board, pieces
                    , numBlack, numWhite
                    ,timeBlack, timeWhite]

{- Draw for individual components -}

-- | Draws the empty board
drawBoardBMP :: Int      -- ^ The size of the board
             -> IO Picture
drawBoardBMP sz = do
  emptyPieces <- mapM (drawEmptyPiece sz) [(x,y) | x <- [0..(sz-1)], y <- [0..(sz-1)]]
  return $ pictures emptyPieces


-- | Gets a string stating the outcome of a game based on a board
getWinner :: Board   -- ^ The board to evaluate to find winner
          -> Int     -- ^ The current time left for black
          -> Int     -- ^ The current time left for white
          -> String  -- Returns a string stating the winner
getWinner b btime wtime | btime <= 0 = "Black ran out of time so White wins!"
                        | wtime <= 0 = "White ran out of time so Black wins!"
                        | x == y     = "Both players have the same number of pc so the game is a draw!"
                        | x > y      = "Black has most pc so wins!"
                        | otherwise  = "White has most pc so wins!"
  where (x,y) = checkScore b


-- | Draws text
drawText :: String   -- ^ String to draw
         -> Float    -- ^ x coordinate to draw at
         -> Float    -- ^ y coordinate to draw at
         -> IO Picture
drawText text x y = return $ scale 0.25 0.25 $ translate x y $ Text text


-- | Draw hint pc on the board
drawHints :: Int         -- ^ The size of the board on which hints are being drawn
          -> [Position]  -- ^ The list of positions to put hint pc
          -> IO Picture
drawHints _ [] = return Blank
drawHints sz ((x,y):ps) = do
  hints <- drawHints sz ps
  return $ pictures [ (translate (squarePosX sz $ fromIntegral(x)) (squarePosY sz $ fromIntegral(y)) $ scale (scaleFactor sz) (scaleFactor sz) hintPiece)
                    , hints
                    ]

-- | Draw an empty space onto the board
drawEmptyPiece :: Int       -- ^ The size of the board
               -> Position  -- ^ The position draw the empty space on the board
               -> IO Picture
drawEmptyPiece sz (x,y) = 
  return $ translate (squarePosX sz $ fromIntegral(x)) (squarePosY sz $ fromIntegral(y)) $ scale (scaleFactor sz) (scaleFactor sz) blankPiece


-- | Draws pc onto the boards
drawPiecesBMP :: Int                -- ^ The size of the board 
              -> [(Position, Col)]  -- ^ List of positions and colours for drawing pc
              -> IO Picture
drawPiecesBMP _ [] = return Blank
drawPiecesBMP sz (((x,y),col):ps) = do
  piece   <- drawPieceBMP sz (squarePosX sz $ fromIntegral(x)) (squarePosY sz $ fromIntegral(y)) col
  pieces  <- drawPiecesBMP sz ps
  return $ pictures [piece, pieces]

-- | Draw a single piece onto the board
drawPieceBMP :: Int      -- ^ The size of the board
             -> Float    -- ^ x position to draw piece
             -> Float    -- ^ y position to draw piece
             -> Col      -- ^ colour of piece that is to be drawn
             -> IO Picture
drawPieceBMP sz x y Black = 
  return $ translate x y $ scale (scaleFactor sz) (scaleFactor sz) blackPiece
drawPieceBMP sz x y White =
  return $ translate x y $ scale (scaleFactor sz) (scaleFactor sz) whitePiece
