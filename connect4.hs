import PlayingGamesAB (Player(..), alphaBetaMax, evaluate)
import qualified Data.Map as Map
import System.IO (hFlush, stdout)

-- Datenstrukturen
data Cell = Empty | X | O deriving (Eq, Ord, Show)
type Column = [Cell]
type Board = [Column]

type Cache = Map.Map Board (String, Int)

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
finished board = isWin board X || isWin board O || all full board
  where full col = all (/= Empty) col

-- Gewinnprüfung
isWin :: Board -> Cell -> Bool
isWin board p = any (checkDirection p board) directions
  where directions = [(1,0), (0,1), (1,1), (1,-1)]

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
utility :: Board -> Int
utility board
  | isWin board X = 1
  | isWin board O = -1
  | otherwise     = 0

-- Beste KI-Aktion berechnen (mit Cache)
bestMove :: Int -> Board -> Board
bestMove depth board = case moves of
  (x:_) -> x
  []    -> board
  where
    successors = nextStates board MaxPlayer
    scored = [ (s, snd (evaluate finished utility nextStates s (alphaBetaMax finished utility nextStates) (-1) 1 Map.empty)) | s <- successors ]
    bestScore = maximum (map snd scored)
    moves = [s | (s, val) <- scored, val == bestScore]

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

-- Menschlicher Zug
getHumanMove :: Board -> IO Int
getHumanMove board = do
  putStr "Dein Zug (Spalte 0-6): "
  hFlush stdout
  input <- getLine
  case reads input of
    [(col, "")] | col >= 0 && col < cols && Empty `elem` (board !! col) -> return col
    _ -> do
      putStrLn "Ungültiger Zug, bitte erneut versuchen."
      getHumanMove board

-- Anwendung eines Zuges
makeMove :: Board -> Int -> Player -> Board
makeMove board col player =
  take col board ++ [newCol] ++ drop (col + 1) board
  where
    Just newCol = dropToken (board !! col) (playerToCell player)

-- Spielschleife
gameLoop :: Board -> Player -> Int -> IO ()
gameLoop board player maxDepth = do
  printBoard board
  if finished board
    then case utility board of
      1  -> putStrLn "Die KI (X) gewinnt!"
      -1 -> putStrLn "Du (O) gewinnst!"
      _  -> putStrLn "Unentschieden!"
    else case player of
      MinPlayer -> do
        col <- getHumanMove board
        let board' = makeMove board col MinPlayer
        gameLoop board' MaxPlayer maxDepth
      MaxPlayer -> do
        putStrLn "Die KI denkt nach..."
        let board' = bestMove maxDepth board
        gameLoop board' MinPlayer maxDepth

-- Hauptfunktion
main :: IO ()
main = do
  putStrLn "Willkommen bei 4 Gewinnt! Du spielst mit O gegen die KI (X)."
  putStr "Bitte maximale Suchtiefe der KI eingeben (z. B. 5): "
  hFlush stdout
  input <- getLine
  let maxDepth = case reads input of
        [(n, "")] | n > 0 -> n
        _ -> 5
  putStrLn $ "KI sucht bis Tiefe " ++ show maxDepth ++ ". Viel Erfolg!"
  gameLoop initialState MinPlayer maxDepth