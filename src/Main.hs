module Main where

import System.Environment

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Board
import Draw
import Input
import AI

-- 'play' starts up a graphics window and sets up handlers for dealing
-- with inputs and updating the world state.
--
-- 'drawWorld' converts the world state into a gloss Picture
--
-- 'handleInput' is called whenever there is an input event, and if it is
-- a human player's turn should update the board with the move indicated by
-- the event
--
-- 'updateWorld' is called 10 times per second (that's the "10" parameter)
-- and, if it is an AI's turn, should update the board with an AI generated
-- move

--main :: IO ()
--main = do args <- getArgs
--          play (InWindow "Othello" (1200, 800) (10, 10)) 
--               black 
--               10
--               (initWorld args) -- in Board.hs
--               drawWorldBMP
--               --drawWorld        -- in Draw.hs
--               handleInput      -- in Input.hs
--               updateWorld      -- in AI.hs


main :: IO ()
main = do 
	args <- getArgs
	playIO 	(InWindow "Othello" (1200,800) (10, 10))
	  		black
	  		10
	  		(initWorld args)
	  		drawWorldIO
	  		handleInputIO
	  		updateWorldNetwork
	  		--updateWorldIO

