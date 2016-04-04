module Input(handleInput) where

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
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) (World (Board size passes pieces) t)
	= case (makeMove (Board size passes pieces) (snapX x, snapY y) t) of
		Just b  -> trace ("Left button pressed at: " ++ show (snapX x, snapY y)) World b (other t)
		Nothing -> trace ("Invalid move. Left button pressed at: " ++ show (snapX x, snapY y)) (World (Board size passes pieces) t)

    -- = trace ("Left button pressed at: " ++ show (snapX x, snapY y)) World (Board size (passes+1) (((snapX x, snapY y), t):pieces)) (other t)
handleInput (EventKey (Char k) Down _ _) w
    = trace ("Key " ++ show k ++ " down") w
handleInput (EventKey (Char k) Up _ _) w
    = trace ("Key " ++ show k ++ " up") w
handleInput e w = w


--Snaps the x mouse coordinate to the x grid coordinate
--snapX = floor((x + gridPos)/rectSize)
snapX :: Float -> Int
snapX x = floor((x + 400.0)/100.0)

--Snaps they mouse coordinate to the y grid coordinate
--snapY = floor((gridPos - y)/rectSize)
snapY :: Float -> Int
snapY y = floor((400.0 - y)/100.0)

{- Hint: when the 'World' is in a state where it is the human player's
 turn to move, a mouse press event should calculate which board position
 a click refers to, and update the board accordingly.

 At first, it is reasonable to assume that both players are human players.
-}

