module Draw(drawWorldBMP, width, gridPos, rectSize, pieceSize) where

import Graphics.Gloss
import Board
import Data.Tuple

import Debug.Trace
import System.IO.Unsafe (unsafePerformIO)

width = 800.0
height = 800.0
gridPos = width/2.0

rectSize :: Int -> Float
rectSize sz = width / (fromIntegral sz)

pieceSize :: Int -> Float
pieceSize sz = ((rectSize sz)/ 2.0)

-- | Translate between our custom 'Col' and the 'Color' needed for drawing
getCol :: Col -> Color
getCol Black = black
getCol White = white

{- Drawing with Bitmap images -}


-- | converts an x position to an x position on the GUI for squares
squarePosX :: Int
           -> Float -- ^ The position to be converted
           -> Float -- ^ Returns the converted position
squarePosX s x = -gridPos + (pieceSize s) + ((rectSize s)* x) 

-- | converts an y position to an y position on the GUI for squares
squarePosY :: Int
           -> Float -- ^ The position to be converted 
           -> Float -- ^ Returns the converted position
squarePosY s y = gridPos - (pieceSize s) - ((rectSize s)* y) 

defaultSquareWidth = 8

scaleFactor :: Int -> Float
scaleFactor sz = defaultSquareWidth / fromIntegral(sz)

bmp :: FilePath -> Picture
bmp file =  unsafePerformIO $ loadBMP file

blackPiece :: Picture
blackPiece = bmp "res/black2.bmp"

whitePiece :: Picture 
whitePiece = bmp "res/white2.bmp"

blankPiece :: Picture
blankPiece = bmp "res/blank.bmp"

hintPiece :: Picture
hintPiece = bmp "res/hint2.bmp"

gameOverScreen :: Picture
gameOverScreen = bmp "res/gameover.bmp"

drawWorldBMP :: World -> Picture

-- Draws hints for reversi start
drawWorldBMP (World (Board sz ps pieces) turn _ _ _ True True _ ) = pictures [ (drawBoardBMP sz)
                                                                      , (drawHints sz (checkStart (Board sz ps pieces)))
                                                                      , (drawPiecesBMP sz pieces)
                                                                      ]

--Draw hints
drawWorldBMP (World (Board sz ps pieces) turn _ _ _ True _ _) = pictures [ (drawBoardBMP sz)
                                                                      , (drawHints sz (checkAvailable (Board sz ps pieces) (0,0) turn))
                                                                      , (drawPiecesBMP sz pieces)
                                                                      ]

--Draw gameover
drawWorldBMP (World (Board sz ps pieces) turn _ _ _ _ _ True ) = pictures [ gameOverScreen, (drawText (getWinner (Board sz ps pieces)) 0 0)]

-- Draw otherwise
drawWorldBMP (World (Board sz ps pieces) _ _ _ _ _ _ _ ) = 
	pictures 	[ (drawBoardBMP sz)
				, (drawPiecesBMP sz pieces)
				, drawText ("Black: " ++ (show $ evaluate (Board sz ps pieces) Black)) (-2300) (-500)
				, drawText ("White: " ++ (show $ evaluate (Board sz ps pieces) White)) (-2300) 0
				]


drawBoardBMP :: Int -> Picture
drawBoardBMP sz = pictures (map (drawEmptyPiece sz) [(x,y) | x <- [0..(sz-1)], y <- [0..(sz-1)]])



getWinner :: Board -> String
getWinner b | x == y    = "Game is a draw"
            | x > y     = "Black wins!"
            | otherwise = "White wins!"
  where (x,y) = checkScore b


drawText :: String -> Float -> Float -> Picture
drawText text x y = scale 0.25 0.25 $ translate x y $ Text text


drawHints :: Int -> [Position] -> Picture
drawHints _ [] = Blank
drawHints sz ((x,y):ps) = pictures [ (translate (squarePosX sz $ fromIntegral(x)) (squarePosY sz $ fromIntegral(y)) $ scale (scaleFactor sz) (scaleFactor sz) hintPiece)
                                , (drawHints sz ps)
                                ]

drawEmptyPiece :: Int -> Position -> Picture
drawEmptyPiece sz (x,y) = translate (squarePosX sz $ fromIntegral(x)) (squarePosY sz $ fromIntegral(y)) $ scale (scaleFactor sz) (scaleFactor sz) blankPiece


drawPiecesBMP :: Int -> [(Position, Col)] -> Picture 
drawPiecesBMP _ [] = Blank
drawPiecesBMP sz (((x,y),col):ps) = pictures [drawPieceBMP sz (squarePosX sz $ fromIntegral(x)) (squarePosY sz $ fromIntegral(y)) col, drawPiecesBMP sz ps]

drawPieceBMP :: Int -> Float -> Float -> Col -> Picture
drawPieceBMP sz x y Black = translate x y $ scale (scaleFactor sz) (scaleFactor sz) blackPiece
drawPieceBMP sz x y White = translate x y $ scale (scaleFactor sz) (scaleFactor sz) whitePiece

