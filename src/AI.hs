{-# LANGUAGE MultiWayIf #-}

module AI where

import System.Exit

import Board

import Debug.Trace

data GameTree = GameTree { game_board :: Board,
                           game_turn :: Col,
                           next_moves :: [(Position, GameTree)] }


genAllMoves :: Board -> Col -> [Position]
genAllMoves b c = [(x,y) | x <- [0..(size b - 1)], y <- [0..(size b - 1)]]


generateMoves :: Board -> Col -> [Position]
generateMoves b c = checkAvailable b (0,0) c

-- Given a function to generate plausible moves (i.e. board positions)
-- for a player (Col) on a particular board, generate a (potentially)
-- infinite game tree.
--
-- (It's not actually infinite since the board is finite, but it's sufficiently
-- big that you might as well consider it infinite!)
--
-- An important part of the AI is the 'gen' function you pass in here.
-- Rather than generating every possible move (which would result in an
-- unmanageably large game tree!) it could, for example, generate moves
-- according to various simpler strategies.

buildTree :: (Board -> Col -> [Position]) -- ^ Move generator
             -> Board -- ^ board state
             -> Col -- ^ player to play next
             -> GameTree
buildTree gen b c = let moves = gen b c in -- generated moves
                        GameTree b c (mkNextStates moves)
  where
    mkNextStates :: [Position] -> [(Position, GameTree)]
    mkNextStates [] = []
    mkNextStates (pos : xs)
        = case makeMove b pos c of -- try making the suggested move
               Nothing -> mkNextStates xs -- not successful, no new state
               Just b' -> (pos, buildTree gen b' (other c)) : mkNextStates xs
                             -- successful, make move and build tree from 
                             -- here for opposite player

-- Get the best next move from a (possibly infinite) game tree. This should
-- traverse the game tree up to a certain depth, and pick the move which
-- leads to the position with the best score for the player whose turn it
-- is at the top of the game tree.

-- | Have Yusuki (the greatest Othello player in the world) make a move.
yusukiMove :: Int -- ^ Maximum search depth
            -> GameTree -- ^ Initial game tree
            -> Position
yusukiMove (-1) tree = fst (head (next_moves tree))
yusukiMove 0 (GameTree b c []) = undefined
yusukiMove depth (GameTree b c next_moves) = trace ("makeing best move possible") $ 
    bestPos depth c next_moves 
    where
      --snd $ maximum [(value, pos) | value <- evaluate ((makeMove b ) c), pos <- next_poss]

      bestPos :: Int -> Col -> [(Position, GameTree)] -> Position
      bestPos depth c next_moves = snd $ maximum $ childrenList (depth-1) next_moves c

      --bestPos :: (Position, Int) -> Board -> Col -> [Position] -> (Position, Int)
      --bestPos best_pos b c [] = best_pos

      --bestPos (pos, value) b c (p:ps) = case makeMove b p c of
      --    Nothing -> error("Couldn't get best move")
      --    Just b' -> let x = (evaluate b' c) in 
      --      case () of 
      --        _ | x > value -> trace ("found better move" ++ show(x,p) ++ " better than " ++ show(value, pos)) $ bestPos (p, x) b c ps
      --        _ | otherwise -> bestPos (pos, value) b c ps

--yusukiMove depth (GameTree b c ms) =         

childrenList :: Int -> [(Position, GameTree)] -> Col -> [(Int, Position)]
childrenList _ [] _ = []
childrenList depth ((p,tree):xs) c = ((evaluateChildren depth tree c), p):(childrenList depth xs c)

--getMax :: Int -> [(Position, GameTree)] -> Col -> Int
--getMax maxPieces [] c = maxPieces
--getMax maxPieces ((_, GameTree {game_board = b}):xs) c | e > maxPieces   = getMax e xs
--                                                       | otherwise = getMax maxPieces xs 
--                                                       where
--                                                         e = evaluate b c


makeList :: [(Position, GameTree)] -> Col -> [Int]
makeList [] c = []
makeList ((_, GameTree {game_board = b}):xs) c = (evaluate b c):(makeList xs c)

getAverage :: [Int] -> Int
getAverage [] = -64
getAverage list = sum list `div` length list

evaluateChildren  :: Int -- ^ Depth 
                  -> GameTree
                  -> Col
                  -> Int
evaluateChildren 0 GameTree {next_moves = ms} c = getAverage $ makeList ms c
evaluateChildren depth GameTree {next_moves = children} c = getAverage (mapEvaluate depth children c) 

mapEvaluate :: Int -> [(Position, GameTree)] -> Col -> [Int]
mapEvaluate _ [] _ = []
mapEvaluate depth ((p, tree):xs) c = trace ("evluating... move " ++ show(p)++ "at depth " ++ show(depth)) $ (evaluateChildren (depth - 1) tree c): (mapEvaluate depth xs c)


--yusukiMove 0 gametree = --get best score and return up the tree
--yusukiMove depth gametree = undefined --keep going down to the given depth

-- Update the world state after some time has passed
updateWorld :: Float -- ^ time since last update (you can ignore this)
            -> World -- ^ current world state
            -> World
updateWorld _ (World b c sts bt wt v True) = World b c sts bt wt v True
updateWorld _ (World b c sts bt wt v _) | gameOver b = let (x,y) = checkScore b
                                                           result | x == y    = error "Game is a draw"
                                                                  | x > y     = error "Black wins!"
                                                                  | otherwise = error "White wins!"
                                                       in result
                                        | not (validMovesAvailable b c) = trace ("No valid moves for " ++ show c ++ " so their turn is skipped") World (b {passes = (passes b) + 1}) (other c) sts bt wt v False
                                        | c == Black && bt == Human     = World b {passes = 0} c sts bt wt v False
                                        | c == White && wt == Human     = World b {passes = 0} c sts bt wt v False
                                        | otherwise = let
                                                  tree = buildTree genAllMoves b c
                                                  nextMove = yusukiMove 2 tree in
                                                  case makeMove b nextMove c of
                                                    Nothing -> error("not possible moves not implemented")
                                                    Just b' -> (World (b' {passes = 0}) (other c) sts bt wt v False)

{- Hint: 'updateWorld' is where the AI gets called. If the world state
 indicates that it is a computer player's turn, updateWorld should use
 'getBestMove' to find where the computer player should play, and update
 the board in the world state with that move.

 At first, it is reasonable for this to be a random valid move!

 If both players are human players, the simple version above will suffice,
 since it does nothing.

 In a complete implementation, 'updateWorld' should also check if either 
 player has won and display a message if so.
-}


