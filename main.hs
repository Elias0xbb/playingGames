{-

AUTHORS: Elias Kiene and Jan Hampel
DATE: 2025-05-15

Main program to play Connect 4 via the PlayingGames module.

-}


import PlayingGames (playGame)


main :: IO ()
main = do
    playGame