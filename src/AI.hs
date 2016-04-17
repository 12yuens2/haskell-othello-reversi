module AI where

import System.Exit
import Network.Socket hiding (sendAll, recv)
import Network.Socket.ByteString.Lazy
import Data.Binary

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
      bestPos depth c next_moves = trace ("--" ++ show(childrenList (depth-1) next_moves c)) snd $ maximum $ childrenList (depth-1) next_moves c


childrenList :: Int -> [(Position, GameTree)] -> Col -> [(Int, Position)]
childrenList _ [] _ = []
--childrenList depth ((p,tree):xs) c = ((evaluateChildren depth tree c (other c)), p):(childrenList depth xs c)
childrenList depth ((p,tree):xs) c = ((evaluateChildren depth tree c c), p):(childrenList depth xs c)


evaluateChildren  :: Int -- ^ Depth 
                  -> GameTree
                  -> Col       -- ^ target colour
                  -> Col       -- ^ current turn
                  -> Int
evaluateChildren 0     GameTree {game_board = b} target current = yusukiEvaluate b target
evaluateChildren depth GameTree {next_moves = ms} target current | target == current = getMin $ makeList depth ms target current
                                                                 | otherwise         = getMax $ makeList depth ms target current

makeList :: Int                      -- ^ depth
         -> [(Position, GameTree)]   -- ^ list of positions and gametrees they create
         -> Col                      -- ^ Colour target
         -> Col                      -- ^ colour of current turn
         -> [Int]
makeList _ [] _ _= []
makeList depth ((_,tree):xs) target current = (evaluateChildren (depth-1) tree target (other current)):(makeList depth xs target current)

-- Possibly need to change cases for no possible moves as no moves on either side good be either good or bad
-- having no moves is bad
getMax :: [Int] -> Int
getMax [] = -10000 
getMax list = maximum list

-- opponent having no moves is good
getMin :: [Int] -> Int
getMin [] = 10000
getMin list = minimum list


-- can probably restructure function to be nicer
updateWorldNetwork _ w@(World _ c _ _ _ _ _ _ _ True _ sd sk) 
            = case sk of
                Nothing -> return w
                Just s  | sd && c == White ||
                          not sd && c == Black -> withSocketsDo $
                                                    -- must check for end of reversi start
                                                    do inputByteString <- recv s 65536
                                                       let b = decode (inputByteString)
                                                       return $ w {board = b, 
                                                                   turn = (other c), 
                                                                   chooseStart = (length (pieces b) < 4)
                                                                  }
                        | otherwise -> return w
updateWorldNetwork _ w@(World b c sts bt wt btime wtime p v r go sd sk) 
            | gameOver b || btime <= 0 || wtime <= 0 = return (World b c sts bt wt btime wtime p v r True sd sk)
            | p                                      = return $ World b c sts bt wt btime wtime p v r go sd sk
            | not (validMovesAvailable b c) = trace ("No valid moves for " ++ show c ++ " so their turn is skipped") return (World (b {passes = (passes b) + 1}) (other c) sts bt wt btime wtime p v False go sd sk)
            | c == Black && bt == Human     = return (World b c sts bt wt (btime-10) wtime p v False go sd sk)
            | c == White && wt == Human     = return (World b c sts bt wt btime (wtime-10) p v False go sd sk)
            | c == White && wt == AI ||
              c == Black && bt == AI = let
                          tree = buildTree generateMoves b c
                          nextMove = yusukiMove 5 tree in
                                     case makeMove b nextMove c of
                                          Nothing -> error("not possible moves not implemented")
                                          Just b' -> return $ (World b' (other c) sts bt wt btime wtime p v False go sd sk)
            -- Find better way of getting just here
            -- WHOLE SECTION HERE NEEDS IMPROVED possibly pattern match again
            | otherwise = case sk of
                Nothing -> return w
                Just s  | sd && c == White ||
                          not sd && c == Black -> withSocketsDo $
                                                    do inputByteString <- recv s 65536
                                                       return $ World (decode inputByteString) (other c) sts bt wt btime wtime p v r go sd sk 
                        | otherwise -> return w

