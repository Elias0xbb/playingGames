import PlayingGames

data Cell = Empty | X | O deriving (Eq, Show)
type Board = [Cell]

initialState :: Board
initialState = [Empty, Empty, Empty,
                 Empty, Empty, Empty,
                 Empty, Empty, Empty]

-- takes a current state and player and returns a set of all possible next states
nextStates :: Board -> Player -> [Board]
nextStates board player = [makeMove board i player | i <- [0..8], board !! i == Empty]
  where
    makeMove b i p = take i b ++ [playerToCell p] ++ drop (i + 1) b
    playerToCell MaxPlayer = X
    playerToCell MinPlayer = O

-- checks if the game is finished (i.e., if a player won or the game is a draw)
finished :: Board -> Bool
finished board = any (== True) [isWin board X, isWin board O, isDraw board]

-- checks if a player of given cell (X or O) has won by checking all possible 
-- winning combinations (rows, columns, diagonals)
isWin :: Board -> Cell -> Bool
isWin b p = any (== True) [all (== p) (getSubArray b wc) | wc <- winningCombinations]
    where
    winningCombinations = [[0, 1, 2], [3, 4, 5], [6, 7, 8],
                           [0, 3, 6], [1, 4, 7], [2, 5, 8],
                           [0, 4, 8], [2, 4, 6]]

isDraw :: Board -> Bool
isDraw b = (all (/= Empty) b) && (not (isWin b X)) && (not (isWin b O))

-- indexes an array by an array of indices and returns the corresponding values
getSubArray :: [a] -> [Int] -> [a]
getSubArray xs is = [xs !! i | i <- is, i < length xs]

-- returns 1 if player X wins, -1 if player X loses, and 0 if it's a draw given the state b is a terminal state
utility :: Board -> Int
utility b
    | isWin b X = 1
    | isWin b O = -1
    | otherwise = 0