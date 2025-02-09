# Sudoku Hidato

Hidato Sudoku is a puzzle game where the goal is to fill a board with consecutive numbers that connect horizontally, vertically, or diagonally. The smallest number is 1, and the largest number corresponds to the total number of squares on the board. Each number’s predecessor and successor must be in adjacent squares.

This project includes a solver and generator for Hidato puzzles, implemented in Haskell.

## Features

- **Generates** random Hidato puzzles.
- **Solves** Hidato puzzles.
- Written in **Haskell**.

## How to Run

1. Open the Haskell compiler (`ghci`) in the project directory.
2. Load the main file:
   ```haskell
   :load main
   ```
3. To generate a board with dimensions n and m, use:
```haskell
board <- generate n m
```
4. To solve the generated board, use:
```haskell
solve board
```
## Installation
Make sure you have Haskell installed. You can run the following in your terminal to install it:

``` bash
sudo apt-get install haskell-platform
```
Clone the repository:
```bash
git clone https://github.com/lorainemg/Sudoku-Hidato.git
cd Sudoku-Hidato
```
## Report
For a detailed report on the project, please visit: [Report PDF](https://github.com/lorainemg/Sudoku-Hidato/blob/main/doc/report.pdf)
