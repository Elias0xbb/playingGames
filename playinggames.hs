module PlayingGames(
  Player(..), 
  getBestMoves,
  getMaxValue,
  getMinValue,
  ) where

data Player = MaxPlayer | MinPlayer deriving (Eq, Show)


getMaxValue :: (s -> Player -> [s]) -- nextStates
         -> (s -> Bool)          -- finished
         -> (s -> Int)           -- utility
         -> s -> Int
getMaxValue nextStates finished utility s
    | finished s = utility s
    | otherwise = maximum [minValue ns | ns <- nextStates s MaxPlayer]
  where
    minValue = getMinValue nextStates finished utility

  
getMinValue :: (s -> Player -> [s]) -- nextStates
         -> (s -> Bool)          -- finished
         -> (s -> Int)           -- utility
         -> s -> Int
getMinValue nextStates finished utility s
    | finished s = utility s
    | otherwise = minimum [maxValue ns | ns <- nextStates s MinPlayer]
  where
    maxValue = getMaxValue nextStates finished utility

-- subset of all possible next states the first player can take
-- which have have the optimal (highest) maxValue equal to maxValue of the current state
getBestMoves :: (s -> Player -> [s]) -- nextStates
          -> (s -> Int)           -- maxValue
          -> (s -> Int)           -- minValue
          -> s -> [s]
getBestMoves nextStates maxValue minValue s = [ns | ns <- nextStates s MaxPlayer, minValue ns == bestValue]
  where
    bestValue = maxValue s

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