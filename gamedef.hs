{-

AUTHORS : Elias Kiene and Jan Hampel
DATE    : 2025-05-15


Module GameDef
=============
This module implements the game logic for Connect 4 (Connect N), including the board representation, 
move generation, win/draw detection, utility and heuristic evaluation, and user interaction functions. 
It provides all necessary types and functions to play Connect 4 (/N) with the PlayingGames module.

Exported functions and types:

- Player(..)      -> The player type, representing the two players (MaxPlayer and MinPlayer).
- State: Type     -> alias for the board state.
- nextStates      -> Generates all possible next board states for a given player.
- finished        -> Checks if the game is finished (win or draw occurred).
- isWin           -> Checks if a given player has won.
- isDraw          -> Checks if the game is a draw.
- utility         -> Returns the utility value of a board as described in Prof. Stroetmann's AI lecture.
- initialState    -> The initial empty board state.
- heuristic       -> Estimates the value of a non-terminal board state.
- getHumanMove    -> Prompts the human player for a valid move.
- makeMove        -> Applies a move for a player to the board.
- initGame        -> Initializes the game and prompts for AI search depth.
- dispState       -> Displays the board in a human-readable format.

-}

module GameDef(
    Player(..),
    State,
    nextStates,
    finished,
    isWin,
    isDraw,
    utility,
    initialState,
    heuristic,
    getHumanMove,
    makeMove,
    initGame,
    dispState,
) where


import qualified Data.Map as Map
import System.IO (hFlush, stdout)


data Player = MaxPlayer | MinPlayer deriving (Ord, Eq, Show)
data Cell = Empty | X | O deriving (Ord, Eq, Show)
type Column = [Cell]
type Board = [Column]
type State = Board -- redefinition / abstraction for PlayingGames module


-- Game parameters:
-- Connect 4 is played on a 7 columns by 6 rows board
-- with a winning condition of at least 4 equal non-empty cells in 
-- a consecutive line (horizontal, vertical, or diagonal).
rows, cols, connectN :: Int
rows = 6
cols = 7
connectN = 4

-- Initial state of the game (empty board)
-- The board is represented as a list of columns, each column being a list of cells.
initialState :: Board
initialState = replicate cols (replicate rows Empty)


-- Maps players to their respective cells/tokens.
playerToCell :: Player -> Cell
playerToCell MaxPlayer = X
playerToCell MinPlayer = O


-- Places a token in the 'lowest' empty cell of a column.
-- Returns Nothing if this is not possible due to the column being full.
-- Otherwise, the new column is returned.
dropToken :: Column -> Cell -> Maybe Column
dropToken col token =
  if Empty `elem` col
  then Just (placeToken col)
  else Nothing
  where
    placeToken (Empty:rest) = token : rest
    placeToken (c:rest) = c : placeToken rest
    placeToken [] = []

-- Generates all possible next board configurations for a current board and player.
nextStates :: Board -> Player -> [Board]
nextStates board player =
  [take i board ++ [newCol] ++ drop (i+1) board
    | (i, col) <- zip [0..] board
    , Just newCol <- [dropToken col (playerToCell player)]]

-- Checks if the game is finished by testing if either player has won 
-- or if the board is full.
finished :: Board -> Bool
finished board = isWin board MaxPlayer || isWin board MinPlayer || all full board
  where full col = all (/= Empty) col

-- Checks if a player has won by checking all possible winning combinations
isWin :: Board -> Player -> Bool
isWin board p = any (checkDirection (playerToCell p) board) directions
  where directions = [(1,0), (0,1), (1,1), (1,-1)]

-- Checks if the game is a draw by checking if the board is full without
-- any player winning.
isDraw :: Board -> Bool
isDraw b = finished b && not (isWin b MaxPlayer) && not (isWin b MinPlayer)


-- Checks if any winning lines with direction (dx, dy) exist for cell `cell`.
checkDirection :: Cell -> Board -> (Int, Int) -> Bool
checkDirection cell board (dx, dy) =
  any (hasLine cell board (dx, dy)) [(x,y) | x <- [0..cols-1], y <- [0..rows-1]]


-- Checks if a line (horizontal, vertical, or diagonal) consists of connectN
-- consecutive cells of type cell.
hasLine :: Cell -> Board -> (Int, Int) -> (Int, Int) -> Bool
hasLine cell board (dx, dy) (x, y) =
  all (== Just cell) [getCell board (x + i*dx, y + i*dy) | i <- [0..connectN-1]]


-- Retrieves a cell from the board at the specified coordinates (x, y)
-- Returns Nothing if the coordinates are out of bounds
getCell :: Board -> (Int, Int) -> Maybe Cell
getCell board (x, y)
  | x < 0 || x >= cols = Nothing
  | y < 0 || y >= rows = Nothing
  | otherwise = Just ((board !! x) !! y)


-- Utility function as defined in the AI lecture
utility :: Board -> Float
utility board
  | isWin board MaxPlayer = 1.0
  | isWin board MinPlayer = -1.0
  | otherwise     = 0.0


-- Simple heuristic function for guessing / estimating the value of a non-terminal state.
-- Every (connectN-1) X's forming a connectN-long line together with an empty cell
-- increment the heuristic by +0.1 while every (connectN-1) O's forming a connectN-long line
-- together with an empty cell decrement the heuristic by -0.1.
heuristic :: Board -> Float
heuristic board = sum [scoreLine line | line <- allLines board]
  where
    scoreLine :: [Cell] -> Float
    scoreLine cells
      | length cells < connectN = 0
      | otherwise = sum [scoreWindow window | window <- windows connectN cells]

    scoreWindow :: [Cell] -> Float
    scoreWindow window
      | count X == (connectN-1) && count Empty == 1 = 0.1
      | count O == (connectN-1) && count Empty == 1 = -0.1
      | otherwise = 0
      where
        count c = length (filter (== c) window)

    -- Get all possible lines (horizontal, vertical, diagonal) of length >= connectN
    allLines :: Board -> [[Cell]]
    allLines b = horiz ++ vert ++ diag1 ++ diag2
      where
        horiz = [[b !! x !! y | x <- [i..i+connectN-1]] | y <- [0..rows-1], i <- [0..cols-connectN]]
        vert  = [[b !! x !! y | y <- [j..j+connectN-1]] | x <- [0..cols-1], j <- [0..rows-connectN]]
        diag1 = [[b !! (i+k) !! (j+k) | k <- [0..connectN-1]] | i <- [0..cols-connectN], j <- [0..rows-connectN]]
        diag2 = [[b !! (i+k) !! (j-k) | k <- [0..connectN-1], j-k >= 0] | i <- [0..cols-connectN], j <- [connectN-1..rows-1]]

    -- Sliding window of size n
    windows :: Int -> [a] -> [[a]]
    windows n xs
      | length xs < n = []
      | otherwise = take n xs : windows n (tail xs)


-- String-representation of cells for printing the board
showCell :: Cell -> String
showCell Empty = ".|"
showCell X     = "X|"
showCell O     = "O|"


-- Prints the board in a human-readable format
-- by printing the columns from top to bottom
-- and the rows from left to right.
-- The board is printed in a grid format
-- with the columns numbered from 0 to 6
-- for ease of move-selection.
dispState :: Board -> IO ()
dispState board = do
    putStrLn "+-------------+"
    mapM_ (putStrLn . ("|" ++) . concatMap showCell) (reverse (transpose board))
    putStrLn "+-+-+-+-+-+-+-+"
    putStrLn "'0'1'2'3'4'5'6'"
    where
        transpose :: [[a]] -> [[a]]
        transpose ([]:_) = []
        transpose x = map head x : transpose (map tail x)


-- Repeatedly prompt player for a move until a valid move is entered.
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


-- Applies a move to the board by placing the respective player's token in the specified column.
makeMove :: Board -> Int -> Player -> Board
makeMove board col player =
  take col board ++ [newCol] ++ drop (col + 1) board
  where
    Just newCol = dropToken (board !! col) (playerToCell player)

-- Initializes the game by printing welcome message,
-- prompting for the progressive deepening depth limit
-- and printing the initial board. 
initGame :: IO Int
initGame = do
    putStrLn "Welcome to Connect 4!"
    putStrLn "The board has 7 columns and 6 rows."
    putStrLn "The player who first gets 4 consecutive tokens in a horizontal, vertical, or diagonal line wins."
    putStrLn "You are playing as the first player 'X' against the AI 'O'."
    putStrLn ""
    putStrLn $ "Please enter the maximum search depth for the AI (recommended 3-8): "
    maxDepth <- getMaxDepth
    dispState initialState
    return maxDepth
  where
    getMaxDepth :: IO Int
    getMaxDepth = do
        input <- getLine
        case reads input of
            [(depth, "")] | depth > 0 -> return depth
            _ -> do
                putStrLn "Ungültige Eingabe. Bitte erneut versuchen."
                getMaxDepth
