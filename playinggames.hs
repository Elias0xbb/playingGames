{-

AUTHORS : Elias Kiene and Jan Hampel
DATE    : 2025-05-15


Module PlayingGames
===================
This module implements the minimax algorithm with alpha-beta pruning and progressive deepening for playing two player games,
as well as interaction logic for a human vs. computer match of any game defined in the imported module GameDef.

Exported functions and types:

- Player(..)       -> The player type (MaxPlayer or MinPlayer).
- initialState     -> The initial state of the game.
- alphaBetaMax     -> Alpha-beta pruning for maximizing player.
- alphaBetaMin     -> Alpha-beta pruning for minimizing player.
- evaluate         -> Evaluation with memoization and alpha-beta pruning.
- pdEvaluate       -> Progressive deepening evaluation.
- playGame         -> Main function to start and play the game.

-}


module PlayingGames(
    Player(..),
    initialState,
    alphaBetaMax,
    alphaBetaMin,
    evaluate,
    pdEvaluate,
    playGame,
) where

import System.Random (mkStdGen, random)
import qualified Data.Map as Map
import qualified Data.Heap as Hp
import GameDef (
    Player(..), 
    State, 
    initialState, 
    finished, 
    utility, 
    nextStates, 
    heuristic, 
    dispState, 
    makeMove, 
    getHumanMove, 
    isWin,
    isDraw,
    initGame,)


-- Memoization cache for alpha-beta pruning with progressive deepening.
-- The cache stores a tuple consisting of the cache flag (=, <=, >=) and the value
-- for tuples of state and depth.
data CacheFlag = CF_LTE | CF_EQ | CF_GTE deriving (Ord, Eq, Show)
type Cache = Map.Map (State, Int) (CacheFlag, Float)


-- alphaBetaMax for minimax algorithm with alpha-beta pruning, caching and 
-- progressive deepening like described in the AI lecture.
-- The cache is recursively updated by looping through the next states via/while calling
-- the local functions `fillHeap` (to sort nextState values) and `loop` (to recursively call evaluate for the next states).
-- Parameters: Cache, current state, depth-limit, alpha, beta
-- Returns: (Cache, Value)
alphaBetaMax :: Cache -> State -> Int -> Float -> Float -> (Cache, Float)
alphaBetaMax cache s limit alpha beta
    | finished s = (cache, utility s)
    | limit == 0 = (cache, heuristic s)
    | otherwise =
        loop cache 
             (fillHeap (Hp.empty :: Hp.MaxPrioHeap Float State) (nextStates s MaxPlayer)) 
             alpha
  where
    fillHeap :: Hp.MaxPrioHeap Float State -> [State] -> Hp.MaxPrioHeap Float State
    fillHeap heap [] = heap
    fillHeap heap (sn:sns) = let
        v = case Map.lookup (sn, limit-2) cache of
            Just (cf, v') -> v'
            Nothing -> -1 
        in fillHeap (Hp.insert (v, sn) heap) sns

    loop cache heap a = case Hp.view heap of
        Nothing -> (cache, a)
        Just ((v, sn), heap') -> let
            (cache', val) = evaluate cache sn (pred limit) alphaBetaMin a beta
            in if val >= beta
               then (cache', val)
               else loop cache' heap' (max a val)


-- alphaBetaMin implementation analogous to alphaBetaMax
-- Parameters: Cache, current state, depth-limit, alpha, beta
-- Returns: (Cache, Value)
alphaBetaMin :: Cache -> State -> Int -> Float -> Float -> (Cache, Float)
alphaBetaMin cache s limit alpha beta
    | finished s = (cache, utility s)
    | limit == 0 = (cache, heuristic s)
    | otherwise = 
        loop cache 
             (fillHeap (Hp.empty :: Hp.MinPrioHeap Float State) (nextStates s MinPlayer)) 
             beta
  where
    fillHeap heap [] = heap
    fillHeap heap (sn:sns) = let
        v = case Map.lookup (sn, limit-2) cache of
            Just (cf, v') -> v'
            Nothing -> 1 
        in fillHeap (Hp.insert (v, sn) heap) sns

    loop cache heap b = case Hp.view heap of
        Nothing -> (cache, b)
        Just ((v, sn), heap') -> let
            (cache', val) = evaluate cache sn (pred limit) alphaBetaMax alpha b
            in if val <= alpha
               then (cache', val)
               else loop cache' heap' (min b val)


-- Evaluate function as described in the AI lecture.
-- Recursively evaluates the current state up to the depth limit
-- and fills the cache in the process.
-- The function checks if the current state is already in the cache.
-- If it is, it returns the cached value.
-- If not, it evaluates the state* and stores the result in the cache.
-- *with (potentially) updated alpha / beta values.
evaluate :: Cache -- cache
            -> State -- current state
            -> Int -- depth limit
            -> (Cache -> State -> Int -> Float -> Float -> (Cache, Float)) -- evaluation function
            -> Float -- alpha
            -> Float -- beta
            -> (Cache, Float) -- (cache, value)
evaluate cache s limit eval alpha beta = 
    case Map.lookup (s, limit) cache of
        Just (cf, v) -> case cf of
            CF_EQ  -> (cache, v)
            CF_LTE -> if v <= alpha
                      then (cache, v)
                      else let 
                        beta' = min beta v
                        (cache', v') = eval cache s limit alpha beta'
                        in (storeCache cache' s limit alpha beta' v', v')
            CF_GTE -> if beta <= v
                      then (cache, v)
                      else let 
                        alpha' = max alpha v
                        (cache', v') = eval cache s limit alpha' beta
                        in (storeCache cache' s limit alpha' beta v', v')
        Nothing ->
            let (cache', v') = eval cache s limit alpha beta
            in (storeCache cache' s limit alpha beta v', v')
    where
        storeCache cache s limit alpha beta v =
            Map.insert (s, limit) (if v <= alpha 
                          then CF_LTE 
                          else if v < beta 
                               then CF_EQ 
                               else CF_GTE, v) cache


-- Progressive deepening evaluation function as described in the AI lecture.
-- It evaluates the current state to progressively deeper levers and stores results in the cache
-- to avoid redundant calculations.
pdEvaluate :: Cache -- cache
              -> State -- current state
              -> Int -- depth limit
              -> (Cache -> State -> Int -> Float -> Float -> (Cache, Float)) -- evaluation function
              -> (Cache, Float) -- (cache, value)
pdEvaluate cache s limit eval = pdLoop cache 0
  where
    pdLoop cache depth = let
        (cache', v) = evaluate cache s depth eval (-1) 1
        in if depth >= limit then (cache', v)
           else if v == (-1) || v == 1 then (cache', v)
                else pdLoop cache' (succ depth)


-- BestMove function as described in the AI lecture.
-- It finds (one of) the best move(s) for the current player given the state of the game.
-- If multiple moves have the same value, the one at `randIdx` is chosen.
-- Random picking can be implemented outside of this function by passing a random index.
bestMove :: Cache -- cache
            -> Int -- depth limit
            -> Int -- random state
            -> State -- current state
            -> Player
            -> (Cache, State)
bestMove cache limit randIdx s p = let
    (cache', bestVal) = pdEvaluate cache s limit (if p == MaxPlayer then alphaBetaMax else alphaBetaMin)
    sns = nextStates s p
    (cache'', nextVs) = getNextVals cache' sns
    bestMoves = [sn | (sn, v) <- zip sns nextVs, v == bestVal]
    in (cache'', bestMoves !! (randIdx `mod` length bestMoves))
  where
    getNextVals cache [] = (cache, [])
    getNextVals cache (sn:sns) = let
        (cache', v) = evaluate cache sn (pred limit) (if p == MaxPlayer then alphaBetaMin else alphaBetaMax) (-1) 1
        (cache'', vs) = getNextVals cache' sns
        in (cache'', v:vs)


-- Determines the winner given the game is finished.
findWinner :: State -> Maybe Player
findWinner s = 
    if isWin s MaxPlayer then Just MaxPlayer
        else if isWin s MinPlayer then Just MinPlayer
    else Nothing


-- Prompt user for a move and execute it.
-- The function checks if the chosen move finishes the game and returns the winner
-- if so. Otherwise, it continues the game by calling the playAiTurn function.
-- For the AI's turn, a pseudo-random number is generated to select different moves
-- for a more interesting gameplay.
playHumanTurn :: Int -> Cache -> Int -> State -> IO (Maybe Player)
playHumanTurn depthLimit cache rng s = do
    humanMove <- getHumanMove s
    let s' = makeMove s humanMove MaxPlayer
    dispState s'
    if finished s' then
        return (findWinner s')
    else
        playAiTurn depthLimit cache (fst $ random $ mkStdGen rng) s'


-- Counterpart to playHumanTurn for the AI player.
-- The function calculates the best move for the AI player using the minimax algorithm
-- with alpha-beta pruning and progressive deepening.
-- It then executes the move and checks if the game is finished.
-- If the game is finished, it returns the winner. Otherwise, it continues the game
-- by calling the playHumanTurn function.
playAiTurn :: Int -> Cache -> Int -> State -> IO (Maybe Player)
playAiTurn depthLimit cache rng s = do
    putStrLn "Calculating move..."
    let (cache', s') = bestMove cache depthLimit rng s MinPlayer
    dispState s'
    if finished s' then
        return (findWinner s')
    else
        playHumanTurn depthLimit cache' rng s'


-- Main function to start and play the game.
-- It initializes the game by asking the player to choose an AI depth limit,
-- prompts the user for their move, and alternates between the human and AI turns
-- while outputting the game states to the console.
-- The game ends when the player/AI wins or the game is a draw.
playGame :: IO ()
playGame = do
    depthLimit <- initGame
    winner <- playHumanTurn depthLimit Map.empty 42 initialState
    putStrLn ""
    putStrLn $ case winner of 
        Just MaxPlayer -> "You win! :D"
        Just MinPlayer -> "You lose! :("
        Nothing -> "It's a draw! :|"
