module PlayingGamesDepthLimited (
  Player(..),
  alphaBetaMax,
  alphaBetaMin,
  heuristic
) where

import Data.List (sortBy)
import Data.Ord (comparing)

-- Spielertypen
data Player = MaxPlayer | MinPlayer deriving (Eq, Ord, Show)

-- Tiefe-limitierte Bewertungsfunktion mit Heuristik
heuristic :: s -> Int
heuristic _ = 0  -- Platzhalter: immer 0 bewerten

-- Maximierer
alphaBetaMax :: (s -> Bool)              -- finished
             -> (s -> Int)               -- utility
             -> (s -> Player -> [s])     -- nextStates
             -> (s -> Int)               -- heuristic
             -> s                        -- state
             -> Int                      -- limit
             -> Int                      -- alpha
             -> Int                      -- beta
             -> Int                      -- value
alphaBetaMax finished utility nextStates heuristic s limit alpha beta
  | finished s      = utility s
  | limit == 0      = heuristic s
  | otherwise       = go (nextStates s MaxPlayer) alpha
  where
    go []     v = v
    go (x:xs) v =
      let val = alphaBetaMin finished utility nextStates heuristic x (limit - 1) v beta
      in if val >= beta then val else go xs (max v val)

-- Minimierer
alphaBetaMin :: (s -> Bool)
             -> (s -> Int)
             -> (s -> Player -> [s])
             -> (s -> Int)
             -> s
             -> Int
             -> Int
             -> Int
             -> Int
alphaBetaMin finished utility nextStates heuristic s limit alpha beta
  | finished s      = utility s
  | limit == 0      = heuristic s
  | otherwise       = go (nextStates s MinPlayer) beta
  where
    go []     v = v
    go (x:xs) v =
      let val = alphaBetaMax finished utility nextStates heuristic x (limit - 1) alpha v
      in if val <= alpha then val else go xs (min v val)
