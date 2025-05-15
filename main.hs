
module PlayingGames(
    Player(..),
    initialState,
    alphaBetaMax,
    alphaBetaMin,
    evaluate,
    pdEvaluate,
    main,
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
-- progressive deepeneng.
-- The cache is recursively updated by looping through the next states via
-- the local function `loop`, which evaluates each state in nextStates
-- according to the minimax algorithm with alpha-beta pruning (cf. AI lecture).
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

findWinner :: State -> Maybe Player
findWinner s = 
    if isWin s MaxPlayer then Just MaxPlayer
        else if isWin s MinPlayer then Just MinPlayer
    else Nothing


playHumanTurn :: Int -> Cache -> Int -> State -> IO (Maybe Player)
playHumanTurn depthLimit cache rng s = do
    humanMove <- getHumanMove s
    let s' = makeMove s humanMove MaxPlayer
    dispState s'
    print $ heuristic s'
    if finished s' then
        return (findWinner s')
    else
        playAiTurn depthLimit cache (fst $ random $ mkStdGen rng) s'

playAiTurn :: Int -> Cache -> Int -> State -> IO (Maybe Player)
playAiTurn depthLimit cache rng s = do
    putStrLn "Calculating move..."
    let (cache', s') = bestMove cache depthLimit rng s MinPlayer
    dispState s'
    print $ heuristic s'
    if finished s' then
        return (findWinner s')
    else
        playHumanTurn depthLimit cache' rng s'



main :: IO ()
main = do
    depthLimit <- initGame
    winner <- playHumanTurn depthLimit Map.empty 42 initialState
    putStrLn $ case winner of 
        Just MaxPlayer -> "You win! :D"
        Just MinPlayer -> "You lose! :("
        Nothing -> "It's a draw! :|"