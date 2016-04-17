module Main where

import System.Environment

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString.Lazy
import Data.Binary

-- import Network.Socket hiding (sendAll, recv)
-- import Network.Socket.ByteString.Lazy
-- import Data.Binary

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


main :: IO ()
main = do 
        args <- getArgs
        w <- initWorld args
        playIO  (InWindow "Othello" (1200,800) (10, 10))
                black
                10
                w
                drawWorldIO
                handleInputIO
                updateWorldNetwork
