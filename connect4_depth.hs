module Main where

import PlayingGamesDepthLimited
import qualified Data.Map as Map
import System.IO (hFlush, stdout)
import Data.List (maximumBy)
import Data.Ord (comparing)

-- Spielzustandstypen
data Cell = Empty | X | O deriving (Eq, Show)
type Column = [Cell]
type Board = [Column]

rows, cols :: Int
rows = 6
cols = 7

initialState :: Board
initialState = replicate cols (replicate rows Empty)

playerToCell :: Player -> Cell
playerToCell MaxPlayer = X
playerToCell MinPlayer = O

-- Einfügen eines Steins in eine Spalte
dropToken :: Column -> Cell -> Maybe Column
dropToken col token =
  if Empty `elem` col
  then Just (placeToken col)
  else Nothing
  where
    placeToken (Empty:rest) = token : rest
    placeToken (c:rest) = c : placeToken rest
    placeToken [] = []

-- Generieren der nächsten Spielzustände
nextStates :: Board -> Player -> [Board]
nextStates board player =
  [ take i board ++ [newCol] ++ drop (i+1) board
  | (i, col) <- zip [0..] board
  , Just newCol <- [dropToken col (playerToCell player)] ]

-- Bewertung des Endzustands
utility :: Board -> Int
utility board
  | isWin board X = 1
  | isWin board O = -1
  | fullBoard board = 0
  | otherwise = 0

-- Gewinnprüfung
isWin :: Board -> Cell -> Bool
isWin board token = any (hasLine token board) directions
  where
    directions = [(1,0), (0,1), (1,1), (1,-1)]

hasLine :: Cell -> Board -> (Int, Int) -> Bool
hasLine token board (dx, dy) =
  any (checkLine token board (dx, dy)) [(x,y) | x <- [0..cols-1], y <- [0..rows-1]]

checkLine :: Cell -> Board -> (Int, Int) -> (Int, Int) -> Bool
checkLine token board (dx, dy) (x, y) =
  all (== Just token) [getCell board (x + i*dx, y + i*dy) | i <- [0..3]]

getCell :: Board -> (Int, Int) -> Maybe Cell
getCell board (x, y)
  | x < 0 || x >= cols = Nothing
  | y < 0 || y >= rows = Nothing
  | otherwise = Just ((board !! x) !! y)

fullBoard :: Board -> Bool
fullBoard = all (all (/= Empty))

-- Heuristik für Nicht-Endzustände
localHeuristic :: Board -> Int
localHeuristic _ = 0 -- Dummy, kann später angepasst werden

-- Beste Aktion für die KI
bestMove :: Int -> Board -> Board
bestMove depth board =
  snd $ maximumBy (comparing fst)
    [ (alphaBetaMin finished utility nextStates localHeuristic s depth (-1) 1, s)
    | s <- nextStates board MaxPlayer ]

-- Endbedingung
finished :: Board -> Bool
finished board = isWin board X || isWin board O || fullBoard board

-- Anzeige
printBoard :: Board -> IO ()
printBoard board = mapM_ putStrLn [ [ showCell (getCellAt x y) | x <- [0..cols-1] ] | y <- reverse [0..rows-1] ]
  where
    getCellAt x y = (board !! x) !! y

showCell :: Cell -> Char
showCell Empty = '.'
showCell X     = 'X'
showCell O     = 'O'

-- Eingabe Mensch
getHumanMove :: Board -> IO Int
getHumanMove board = do
  putStr "Dein Zug (Spalte 0-6): "
  hFlush stdout
  input <- getLine
  case reads input of
    [(col, "")] | col >= 0 && col < cols && Empty `elem` (board !! col) -> return col
    _ -> putStrLn "Ungültig!" >> getHumanMove board

-- Spielzug anwenden
makeMove :: Board -> Int -> Player -> Board
makeMove board col player =
  let Just newCol = dropToken (board !! col) (playerToCell player)
  in take col board ++ [newCol] ++ drop (col + 1) board

-- Spielschleife
gameLoop :: Board -> Player -> Int -> IO ()
gameLoop board player depth = do
  printBoard board
  if finished board
    then case utility board of
      1  -> putStrLn "Die KI gewinnt!"
      -1 -> putStrLn "Du gewinnst!"
      _  -> putStrLn "Unentschieden!"
    else case player of
      MinPlayer -> do
        col <- getHumanMove board
        let board' = makeMove board col MinPlayer
        gameLoop board' MaxPlayer depth
      MaxPlayer -> do
        putStrLn "KI denkt..."
        let board' = bestMove depth board
        gameLoop board' MinPlayer depth

-- Start
main :: IO ()
main = do
  putStrLn "Willkommen bei 4 Gewinnt mit Tiefensuche!"
  putStr "Maximale Tiefe (z. B. 4): "
  hFlush stdout
  input <- getLine
  let depth = case reads input of
        [(n, "")] -> n
        _          -> 4
  gameLoop initialState MinPlayer depth
