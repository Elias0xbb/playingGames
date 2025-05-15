# Connect 4 AI with Minimax and Alpha-Beta Pruning

This project implements a simple AI for playing Connect 4 using the minimax algorithm with alpha-beta pruning, memoization, and progressive deepening. The AI is designed to play against a human player in a turn-based fashion, making optimal moves based on the current game state.

## Features

* **Minimax Algorithm**: Implements the minimax algorithm for decision making.
* **Alpha-Beta Pruning**: Optimizes the minimax search by pruning branches that do not need to be explored.
* **Memoization**: Uses a cache to store the evaluation results of board states to avoid redundant computations.
* **Progressive Deepening**: Evaluates the game state at increasing depths for better move decisions.
* **Human vs. AI Gameplay**: Allows a human player to play against an AI using the minimax strategy.

## Project Structure

* **`gamedef.hs`**: Contains the core game description for Connect 4, including the board representation, move generation, win/draw detection, utility, and heuristic evaluation functions.
* **`playinggames.hs`**: Implements the minimax algorithm with alpha-beta pruning and progressive deepening. Also contains the main game loop for human vs. AI gameplay.
* **`main.hs`**: The entry point for the game. It initializes the game and starts the gameplay loop.

## How to Compile and Run

### Prerequisites

Ensure that you have Haskell and the required tools installed. You will need the `ghc` compiler and `cabal` build system.

1. Install GHC (Glasgow Haskell Compiler) and Cabal (Haskell's build tool) if you don't have them installed:

   * Visit [Haskell's installation guide](https://www.haskell.org/downloads/) for instructions.

2. Clone this repository to your local machine:

   ```bash
   git clone https://github.com/Elias0xbb/playingGames.git
   ```

### Building the Project

1. Compile the project via :

   ```bash
   ghc main.hs playinggames.hs gamedef.hs
   ```
    Note that, depending on your haskell installation, you might have to manually install the modules random, containers and heap and possibly specify the `-package containers` and `-package heap` flags when compiling.

2. This will compile the Haskell files and create an executable `main.exe` or `main.out`.

### Running the Game

To start the game, run the following command in your terminal:

```bash
.\main.out
```
or
```powershell
main.exe
```

This will start the Connect 4 game with the AI playing against you. The AI will make its move after you play your turn, and the game will continue until there is a winner or the game ends in a draw.
Before playing, you will be prompted for a maximum computation depth for progressive deepening. It is recommended to choose a limit between 3 and 6.

### Game Flow

1. The game initializes by prompting you to enter a depth limit for the AI's search.
2. You take turns with the AI. You will be prompted to enter your move (a column between 0 and 6).
3. The AI calculates its next move using the minimax algorithm with alpha-beta pruning.
4. The game board is displayed after each move, and the game continues until there is a winner or the game is a draw.

## AI Strategy

The AI uses a **minimax** algorithm with **alpha-beta pruning** to decide on the best move. The search is optimized using **memoization** to avoid recalculating the value of already evaluated board states. The algorithm uses **progressive deepening**, where it evaluates the board at increasing depths until it reaches the specified depth limit and falls back to a simple heuristic for estimating a state's value (by looking for potentially completable lines consisting of 3 Xs/Os and an Empty field).

## Authors

* Elias Kiene
* Jan Hampel
