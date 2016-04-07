module Input(handleInput) where

{-# LANGUAGE MultiWayIf #-}

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI
import Draw

import Debug.Trace

-- Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
--
-- trace :: String -> a -> a
-- 'trace' returns its second argument while printing its first argument
-- to stderr, which can be a very useful way of debugging!
handleInput :: Event -> World -> World
--handleInput (EventMotion (x, y)) w 
--    = trace ("Mouse moved to: " ++ show (x,y)) w
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) (World (Board size passes pc) t sts bt wt v True)
    = case (startMove (Board size passes pc) (snapX size x, snapY size y) t) of
           Just b  -> trace ("Left button pressed at: " ++ show (snapX size x, snapY size y)) (World b (other t) (((Board size passes pc),t):sts) bt wt v (startState (pieces b)))
           Nothing -> trace ("Invalid move. Left button pressed at: " ++ show (snapX size x, snapY size y)) (World (Board size passes pc) t sts bt wt v True)

handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) (World (Board size passes pieces) t sts bt wt v r)
    = case (makeMove (Board size passes pieces) (snapX size x, snapY size y) t) of
        Just b  -> trace ("Left button pressed at: " ++ show (snapX size x, snapY size y)) (World b (other t) (((Board size passes pieces),t):sts) bt wt v r)
        Nothing -> trace ("Invalid move. Left button pressed at: " ++ show (snapX size x, snapY size y)) (World (Board size passes pieces) t sts bt wt v r)
handleInput (EventKey (Char 'u') Down _ _) w
    = undoTurn w
handleInput (EventKey (Char k) Down _ _) w
    = trace ("Key " ++ show k ++ " down") w
handleInput (EventKey (Char k) Up _ _) (World b t sts bt wt v r) 
		| k == 'h' 	= (World b t sts bt wt (not v) r)
	  	| otherwise = (World b t sts bt wt v r)

handleInput e w = w


--Snaps the x mouse coordinate to the x grid coordinate
--snapX = floor((x + gridPos)/rectSize)
snapX :: Int -> Float -> Int
snapX s x = floor((x + gridPos)/(rectSize s))

--Snaps they mouse coordinate to the y grid coordinate
--snapY = floor((gridPos - y)/rectSize)
snapY :: Int -> Float -> Int
snapY s y = floor((gridPos - y)/(rectSize s))

{- Hint: when the 'World' is in a state where it is the human player's
 turn to move, a mouse press event should calculate which board position
 a click refers to, and update the board accordingly.

 At first, it is reasonable to assume that both players are human players.
-}

