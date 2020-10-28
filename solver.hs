module Solver where
import Data.List
import Data.Maybe
import Debug.Trace
import Utils

--------------------------- Finds the position of an element  ---------------------------
findElement :: [[Integer]] -> Integer -> (Int, Int)
findElement cells el = (x, y)
    where
        x = findLine cells el 0
        y = fromJust $ elemIndex el (cells!!x)
        
------------------------- Returns the row of an element in the list -------------------------
findLine :: [[Integer]] -> Integer -> Int -> Int 
findLine cells el pos
    | pos == length cells = -1
    | el `elem` (cells !! pos) = pos
    | otherwise = findLine cells el (pos + 1)

-------------------- Returns the next available num in the board --------------------
nextNum :: Integer -> Board -> [Int] -> [Int] -> Integer
nextNum num board dx dy
    -- si el numero no está en el tablero entonces el es el siguiente numero
    | all (\x -> num `notElem` x) cellList = num
    -- si en cualquiera de sus alrededores esta el numero anterior entonces 
    -- el numero está bien posicionado, seguir buscando el numero
    | valid = nextNum (num + 1) board dx dy  
    -- en otro caso se invalida el tablero porque el numero anterior esta muy lejos
    | otherwise = maxNum board + 1
    where
        cellList = cells board
        (x, y) = findElement cellList num
        valid = any (\x -> x == num - 1) 
            [cellList!!(x+x1)!!(y+y1) | x1 <- dx, y1 <- dy, inRange (x+x1, y+y1) cellList]


------------------------------- Main function to solve a board -------------------------------
solve :: Board -> IO ()
solve board =
    printBoard newBoard
    where
        num = minNum board
        -- Se empieza a llenar el tablero por donde esta el menor elemento
        pos = findElement (cells board) num
        -- se busca el ultimo numero que se debe escribir en el tablero
        zeros = calculateZeros (cells board)
        -- se llama a la funcion recursiva para resolver el tablero
        newBoard = solveR board num pos zeros


------------------------------- Recursive function to solve the board -------------------------------
solveR :: Board -> Integer -> (Int, Int) -> Integer -> Board
solveR board num (x, y) zeros
    -- Si solo falta poner el ultimo numero entonces ya termino
    | zeros == 0 = newBoard
    -- Si todas las casillas alrededor del numero estan ocupadas entonces no puedo poner un numero nuevo y retorno
    | cantContinue (x, y) (dx, dy) (cells board) = Empty
    -- Si el siguiente numero es mayor que 40 entonces es porque se invalido el tablero
    | newNum > 40 = Empty
    -- En otro caso busco la solucion del tablero
    | otherwise = maybeBoard
    where
        -- Arrays direccionales
        dx = [0, 0,  1, 1,  1, -1, -1, -1]
        dy = [1, -1, 0, 1, -1,  1,  0, -1]
        -- Creo el nuevo tablero poniendo el numero en tablero anterior
        cellList = replaceAt (x, y) num (cells board)
        newBoard = Board cellList (minNum board) (maxNum board)
        -- Calculo el siguiente  numero que voy a poner
        -- Si num + 1 no esta en el tablero retorno ese, si no el siguiente numero que no esté
        newNum = nextNum (num+1) newBoard dx dy
        -- Encuentro la posicion del numero anterior
        -- Si me desplacé en newNum tengo que encontrar otro punto de partida 
        -- xq el numero anterior no es el que acabo de poner
        (nX, nY) = findElement cellList (newNum-1)
        -- Busco el siguiente board llamando recursivamente con la matriz direccional
        maybeBoard = recursiveCall 0
        recursiveCall index
            -- Ya probé todas las direcciones
            | index == length dx = Empty
            -- Si no esta en rango o el elemento en la nueva posicion esta ocupado entonces no puedo poner el numero
            | not (inRange (newX, newY) cellList) || cellList!!newX!!newY /= 0 = recursiveCall (index + 1)
            -- Si el numero de ceros conectados es menor que los ceros que quedan entonces no es una solucion valida
            -- Cuando encuentre la primera solucion valida retorno
            | solution /= Empty = solution
            -- Si no he encontrado solucion pruebo con otra direccion
            | otherwise = recursiveCall (index + 1)
            where
                -- Calculo la nueva posicion
                (newX, newY) = (nX + dx!!index, nY+dy!!index)
                -- Llamo para buscar si poniendo el siguiente numero en la nueva posicion es solucion
                solution = solveR newBoard newNum (newX, newY) (zeros-1)


sample = Board [
--  0    1   2   3   4   5  6   7  
    [0, 33, 35,  0,  0],         -- 0   
    [0,   0, 24, 22,  0],        -- 1
    [0,   0,  0, 21,  0, 0],     -- 2
    [0,  26,  0, 13, 40, 11],    -- 3
    [27,  0,  0,  0,  9,  0, 1], -- 4
    [-1, -1,  0,  0, 18,  0, 0], -- 5
    [-1, -1, -1, -1,  0,  7, 0, 0], -- 6
    [-1, -1, -1, -1, -1, -1, 5, 0]] --7
    1 40

sample1 = Board [
    [1, 0],
    [0, 4]]
    1 4