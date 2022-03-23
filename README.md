# Sudoku Hidato

Hidato Sudoku is a board with several numbers placed on it and some blank squares. The smallest number will always be 1 and the largest will coincide with the number of squares on the board, both will always be present on the board. The goal is to fill it with consecutive numbers that connect horizontally, vertically, or diagonally, thus forming a sequence of numbers starting with 1 that are immediately connected. For each number, its predecessor and its successor will be in some adjacent box.

In this project is implemented an agent that resolves Hidato and another that generates it. The project is written in Haskell. A full report of the project can be found at: https://github.com/lorainemg/Sudoku-Hidato/blob/main/doc/report.pdf



## Start the game

To start the game you have to open the Haskell compiler (in our case `ghci` was used) in the project directory and execute `:load main`, since `main.hs` is the main file. This file imports the `generate` and `solve` methods that were mentioned earlier. To call `generate` do: `board <- generate n m`, where `n` and `m` are any 2 numbers that represent the dimensions of the board. Since you're dealing with `randomRIO` numbers, the true return value of this method is `Board IO`, so you need to store the value in a variable before execution to access the value of `Board`.

Then, once you have the board, it's executed: `solve board`, `board` being the result obtained by executing the generator. This procedure can be executed repeatedly.