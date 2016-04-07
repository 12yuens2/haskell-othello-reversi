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
rectSize size = width / (fromIntegral size)

pieceSize :: Int -> Float
pieceSize size = ((rectSize size)/ 2.0)

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
scaleFactor size = defaultSquareWidth / fromIntegral(size)

bmp :: FilePath -> Picture
bmp file =  unsafePerformIO $ loadBMP file

blackPiece :: Picture
blackPiece = bmp "res/black.bmp"

whitePiece :: Picture 
whitePiece = bmp "res/white.bmp"

blankPiece :: Picture
blankPiece = bmp "res/blank.bmp"

hintPiece :: Picture
hintPiece = bmp "res/hint.bmp"

drawWorldBMP :: World -> Picture
drawWorldBMP (World (Board size passes pieces) turn _ _ _ True) = pictures [ (drawBoardBMP size)
                                                                      , (drawHints size (checkAvailable (Board size passes pieces) (0,0) turn))
                                                                      , (drawPiecesBMP size pieces)
                                                                      ]

drawWorldBMP (World (Board size passes pieces) _ _ _ _ _) = 
	pictures 	[ (drawBoardBMP size)
				, (drawPiecesBMP size pieces)
				, drawText ("Black: " ++ (show $ evaluate (Board size passes pieces) Black)) (-2300) (-500)
				, drawText ("White: " ++ (show $ evaluate (Board size passes pieces) White)) (-2300) 0
				]


drawBoardBMP :: Int -> Picture
drawBoardBMP size = pictures (map (drawEmptyPiece size) [(x,y) | x <- [0..(size-1)], y <- [0..(size-1)]])



drawText :: String -> Float -> Float -> Picture
drawText text x y = scale 0.25 0.25 $ translate x y $ Text text


drawHints :: Int -> [Position] -> Picture
drawHints _ [] = Blank
drawHints size ((x,y):ps) = pictures [ (translate (squarePosX size $ fromIntegral(x)) (squarePosY size $ fromIntegral(y)) $ scale (scaleFactor size) (scaleFactor size) hintPiece)
                                , (drawHints size ps)
                                ]

drawEmptyPiece :: Int -> Position -> Picture
drawEmptyPiece size (x,y) = translate (squarePosX size $ fromIntegral(x)) (squarePosY size $ fromIntegral(y)) $ scale (scaleFactor size) (scaleFactor size) blankPiece


drawPiecesBMP :: Int -> [(Position, Col)] -> Picture 
drawPiecesBMP _ [] = Blank
drawPiecesBMP size (((x,y),col):ps) = pictures [drawPieceBMP size (squarePosX size $ fromIntegral(x)) (squarePosY size $ fromIntegral(y)) col, drawPiecesBMP size ps]

drawPieceBMP :: Int -> Float -> Float -> Col -> Picture
drawPieceBMP size x y Black = translate x y $ scale (scaleFactor size) (scaleFactor size) blackPiece
drawPieceBMP size x y White = translate x y $ scale (scaleFactor size) (scaleFactor size) whitePiece

