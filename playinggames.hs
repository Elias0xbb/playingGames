
module PlayingGames(
  Player(..), 
  getBestMoves,
  value,
  applyValueToList,
  ) where

import qualified Data.Map as Map
import Data.List (foldl')

data Player = MaxPlayer | MinPlayer deriving (Ord, Eq, Show)


value :: Ord s =>
      (s -> Player -> [s]) -- nextStates
      -> (s -> Bool)          -- finished
      -> (s -> Int)           -- utility
      -> Player
      -> Map.Map (Player, s) Int            -- cache
      -> s -> (Map.Map (Player, s) Int, Int)
value nextStates finished utility player cache s
    | finished s = let v = utility s in (Map.insert (player, s) v cache, v)
    | otherwise = case Map.lookup (player, s) cache of
        Just v  -> (cache, v)
        Nothing -> let
            nextStates' = nextStates s player
            fvalue = value nextStates finished utility (if player == MaxPlayer then MinPlayer else MaxPlayer)
            (newCache, values) = applyValueToList fvalue cache nextStates'
            res = (if player == MaxPlayer then maximum else minimum) values
            in (Map.insert (player, s) res newCache, res)

applyValueToList :: Ord s =>
      (Map.Map (Player, s) Int -> s -> (Map.Map (Player, s) Int, Int)) -- value function
      -> Map.Map (Player, s) Int                         -- cache 
      -> [s] -> (Map.Map (Player, s) Int, [Int])
applyValueToList fvalue cache [] = (cache, [])
applyValueToList fvalue cache (x:xs)
  = (newCache', v:vs)
  where
    (newCache, v) = fvalue cache x
    (newCache', vs) = applyValueToList fvalue newCache xs
  

-- subset of all possible next states the first player can take
-- which have have the optimal (highest) maxValue equal to maxValue of the current state
getBestMoves :: (Ord s) =>
      (s -> Player -> [s])    -- nextStates
      -> Player
      -> (Player -> Map.Map (Player, s) Int -> s -> (Map.Map (Player, s) Int, Int))    -- value function
      -> Map.Map (Player, s) Int    -- cache 
      -> s -> (Map.Map (Player, s) Int, [s])
getBestMoves nextStates player fvalue cache s = (newCache', [s' | (s', v) <- nextStateValuePairs, v == bestValue])
  where
    nextStates' = nextStates s player
    (newCache, nextValues') = applyValueToList (fvalue (if player == MaxPlayer then MinPlayer else MaxPlayer)) cache nextStates'
    nextStateValuePairs = zip nextStates' nextValues'
    (newCache', bestValue) = fvalue player newCache s


{--
-- returns the first best move for the first player (MaxPlayer) given the current state s
-- TODO / possible improvement: return random move instead
makeMove :: (s -> Player -> [s]) -- nextStates
         -> (s -> Bool)          -- finished
         -> (s -> Int)           -- utility
         -> s -> s
makeMove nextStates finished utility s = head (getBestMoves nextStates maxValue minValue s)
  where
    maxValue = getMaxValue nextStates finished utility
    minValue = getMinValue nextStates finished utility

--}