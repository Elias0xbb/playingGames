module GameDefC4(
    Player(..),
    State,
    nextStates,
    finished,
    isWin,
    isDraw,
    utility,
    initialState,
) where


import qualified Data.Map as Map
import System.IO (hFlush, stdout)


data Player = MaxPlayer | MinPlayer deriving (Ord, Eq, Show)
data Cell = Empty | X | O deriving (Ord, Eq, Show)
type Column = [Cell]
type Board = [Column]
type State = Board

-- Spielparameter
rows, cols, connectN :: Int
rows = 6
cols = 7
connectN = 4

-- Anfangszustand
initialState :: Board
initialState = replicate cols (replicate rows Empty)

-- Umwandlung von Spieler in Spielstein
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

-- Erzeuge alle gültigen Folgezustände
nextStates :: Board -> Player -> [Board]
nextStates board player =
  [take i board ++ [newCol] ++ drop (i+1) board
    | (i, col) <- zip [0..] board
    , Just newCol <- [dropToken col (playerToCell player)]]

-- Prüfen, ob das Spiel vorbei ist
finished :: Board -> Bool
finished board = isWin board MaxPlayer || isWin board MinPlayer || all full board
  where full col = all (/= Empty) col

-- Gewinnprüfung
isWin :: Board -> Player -> Bool
isWin board p = any (checkDirection (playerToCell p) board) directions
  where directions = [(1,0), (0,1), (1,1), (1,-1)]

isDraw :: Board -> Bool
isDraw b = finished b && not (isWin b MaxPlayer) && not (isWin b MinPlayer)

checkDirection :: Cell -> Board -> (Int, Int) -> Bool
checkDirection cell board (dx, dy) =
  any (hasLine cell board (dx, dy)) [(x,y) | x <- [0..cols-1], y <- [0..rows-1]]

hasLine :: Cell -> Board -> (Int, Int) -> (Int, Int) -> Bool
hasLine cell board (dx, dy) (x, y) =
  all (== Just cell) [getCell board (x + i*dx, y + i*dy) | i <- [0..connectN-1]]

getCell :: Board -> (Int, Int) -> Maybe Cell
getCell board (x, y)
  | x < 0 || x >= cols = Nothing
  | y < 0 || y >= rows = Nothing
  | otherwise = Just ((board !! x) !! y)

-- Bewertung eines Endzustands
utility :: Board -> Float
utility board
  | isWin board MaxPlayer = 1.0
  | isWin board MinPlayer = -1.0
  | otherwise     = 0.0

-- Visualisierung
showCell :: Cell -> Char
showCell Empty = '.'
showCell X     = 'X'
showCell O     = 'O'

printBoard :: Board -> IO ()
printBoard board =
  mapM_ putStrLn [ [ showCell (getCellAt x y) | x <- [0..cols-1] ] | y <- reverse [0..rows-1] ]
  where
    getCellAt x y = if y < length (board !! x) then (board !! x) !! y else Empty

