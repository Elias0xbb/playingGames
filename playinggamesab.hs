module PlayingGamesAB (
  Player(..),
  getBestMovesWithDepth,
  valueABWithDepth
) where

import qualified Data.Map as Map

data Player = MaxPlayer | MinPlayer deriving (Eq, Ord, Show)

-- Alpha-Beta mit Tiefe
valueABWithDepth :: Ord s =>
     (s -> Player -> [s])  -- nextStates
  -> (s -> Bool)           -- finished
  -> (s -> Int)            -- utility
  -> Int                   -- maxDepth
  -> Player
  -> Map.Map (Player, s) Int
  -> s
  -> Int                   -- alpha
  -> Int                   -- beta
  -> Int                   -- current depth
  -> (Map.Map (Player, s) Int, Int)
valueABWithDepth nextStates finished utility maxDepth player cache s alpha beta depth
  -- 🎯 Endzustände immer zuerst prüfen
  | finished s = let u = utility s in (Map.insert (player, s) u cache, u)
  -- 💡 Tiefe erreicht → abbrechen mit neutralem Wert (oder später Heuristik)
  | depth >= maxDepth = (cache, 0)
  -- 📦 Caching
  | Just v <- Map.lookup (player, s) cache = (cache, v)
  -- 🔁 Suche fortsetzen
  | otherwise = search (nextStates s player) cache alpha beta Nothing
  where
    nextPlayer = if player == MaxPlayer then MinPlayer else MaxPlayer

    search [] c _ _ (Just best) = (Map.insert (player, s) best c, best)
    search [] c _ _ Nothing     = (Map.insert (player, s) 0 c, 0)

    search (x:xs) c a b bestSoFar =
      let (c', val) = valueABWithDepth nextStates finished utility maxDepth nextPlayer c x a b (depth + 1)
          (newBest, newA, newB, prune) = case player of
            MaxPlayer ->
              let best = maybe val (max val) bestSoFar
              in (Just best, max a val, b, max a val >= b)
            MinPlayer ->
              let best = maybe val (min val) bestSoFar
              in (Just best, a, min b val, a >= min b val)
      in if prune
         then (Map.insert (player, s) (unwrap newBest) c', unwrap newBest)
         else search xs c' newA newB newBest

    unwrap (Just v) = v
    unwrap Nothing = error "Tried to unwrap Nothing in valueABWithDepth"

-- Beste Folgezüge berechnen
getBestMovesWithDepth :: Ord s =>
     (s -> Player -> [s])
  -> (s -> Bool)
  -> (s -> Int)
  -> Int                      -- maximale Tiefe
  -> Player
  -> s
  -> (Map.Map (Player, s) Int, [s])
getBestMovesWithDepth nextStates finished utility maxDepth player s =
  (finalCache, [st | (st, val) <- zip successors values, val == bestVal])
  where
    successors = nextStates s player
    nextPlayer = if player == MaxPlayer then MinPlayer else MaxPlayer

    abEval c st = valueABWithDepth nextStates finished utility maxDepth nextPlayer c st (-1000000) (1000000) 1
    (finalCache, values) = foldl (\(c, vs) st ->
      let (c', v) = abEval c st
      in (c', vs ++ [v])) (Map.empty, []) successors

    (_, bestVal) = valueABWithDepth nextStates finished utility maxDepth player finalCache s (-1000000) (1000000) 1
