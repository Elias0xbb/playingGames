{-# LANGUAGE TupleSections #-}

module PlayingGamesAB (
  Player(..),
  evaluate,
  alphaBetaMax,
  alphaBetaMin
) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- Spielertypen
data Player = MaxPlayer | MinPlayer deriving (Eq, Ord, Show)

type CacheFlag = String -- "=", "<=", ">="
type Cache s = Map.Map s (CacheFlag, Int)

-- Evaluierungsfunktion mit Caching (Memoization)
evaluate :: Ord s =>
     (s -> Bool)            -- finished
  -> (s -> Int)             -- utility
  -> (s -> Player -> [s])   -- nextStates
  -> s                      -- aktueller Zustand
  -> (s -> Int -> Int -> Cache s -> (Cache s, Int)) -- Bewertungsfunktion: alphaBetaMax oder Min
  -> Int                    -- alpha
  -> Int                    -- beta
  -> Cache s                -- Cache
  -> (Cache s, Int)
evaluate finished utility nextStates s f alpha beta cache =
  case Map.lookup s cache of
    Just ("=", v) -> (cache, v)
    Just ("<=", v) | v <= alpha -> (cache, v)
                    | otherwise -> f s alpha (min beta v) cache
    Just (">=", v) | v >= beta -> (cache, v)
                    | otherwise -> f s (max alpha v) beta cache
    Nothing ->
      let (cache', v) = f s alpha beta cache
          cache'' = storeCache s alpha beta v cache'
      in (cache'', v)

-- Cache-Strategie laut Spezifikation
storeCache :: Ord s => s -> Int -> Int -> Int -> Cache s -> Cache s
storeCache s alpha beta v cache
  | v <= alpha = Map.insert s ("<=", v) cache
  | v < beta   = Map.insert s ("=",  v) cache
  | otherwise  = Map.insert s (">=", v) cache

-- Maximierer
alphaBetaMax :: Ord s =>
     (s -> Bool)
  -> (s -> Int)
  -> (s -> Player -> [s])
  -> s
  -> Int -> Int
  -> Cache s
  -> (Cache s, Int)
alphaBetaMax finished utility nextStates s alpha beta cache
  | finished s = (cache, utility s)
  | otherwise  = loop (nextStates s MaxPlayer) cache alpha
  where
    loop [] c a = (c, a)
    loop (n:ns) c a =
      let (c', val) = evaluate finished utility nextStates n (alphaBetaMin finished utility nextStates) a beta c
      in if val >= beta
         then (c', val)
         else loop ns c' (max a val)

-- Minimierer
alphaBetaMin :: Ord s =>
     (s -> Bool)
  -> (s -> Int)
  -> (s -> Player -> [s])
  -> s
  -> Int -> Int
  -> Cache s
  -> (Cache s, Int)
alphaBetaMin finished utility nextStates s alpha beta cache
  | finished s = (cache, utility s)
  | otherwise  = loop (nextStates s MinPlayer) cache beta
  where
    loop [] c b = (c, b)
    loop (n:ns) c b =
      let (c', val) = evaluate finished utility nextStates n (alphaBetaMax finished utility nextStates) alpha b c
      in if val <= alpha
         then (c', val)
         else loop ns c' (min b val)