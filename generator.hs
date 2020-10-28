module Generator where
import Data.List
import Data.Maybe
import System.Random
import Utils

------------------------------- Generates a cloudy blank board -------------------------------

cloudBoard :: Integer -> Integer -> Board
cloudBoard n m =  
    Board {cells = [[
        if 
            (((x>(n-3))||(y>(m-3)))&&((x>y+3)||(y>x+3)))||
            (((x>(n-2))||(y>(m-2)))&&((x>y+2)||(y>x+2)))||
            (((x>(n-1))||(y>(m-1)))&&((x>y+1)||(y>x+1))) 
        then (-1) 
        else (0) 
        | x <- [1..n]] | y <- [1..m]], 
        minNum = 1, maxNum = n*m}

------------------------------- Generates a square blank board -------------------------------

squareBoard :: Integer -> Integer -> Board
squareBoard n m =  
    Board {cells = [[0 | _ <- [1..n]] | _ <- [1..m]], minNum = 1, maxNum = n*m}

------------------------------- Generates a mirror blank board -------------------------------

mirrorBoard :: Integer -> Integer -> Board
mirrorBoard n m =  
    Board {cells = [[ if x==y then (-1) else (0)| x <- [1..n]] | y <- [1..m]], minNum = 1, maxNum = n*m}

------------------------------- Main function to fill a board -------------------------------
fill :: Board -> (Int,Int) -> Board
fill board pos = newBoard
    where
        num = minNum board
        -- se busca el ultimo numero que se debe escribir en el tablero
        lastNum = calculateZeros (cells board)
        -- se llama a la funcion recursiva para resolver el tablero
        newBoard = fillR board num pos lastNum

------------------------------- Recursive function to fill the board -------------------------------
fillR :: Board -> Integer -> (Int, Int) -> Integer -> Board
fillR board num (x, y) lastNum
    -- Si solo falta poner el ultimo numero entonces ya termino
    | num == lastNum = newBoard
    -- Si todas las casillas alrededor del numero estan ocupadas entonces no puedo poner un numero nuevo y retorno
    | cantContinue (x, y) (dx, dy) (cells board) = Empty
    -- En otro caso busco la solucion del tablero
    | otherwise = maybeBoard
    where
        -- Arrays direccionales
        dx = [0, 0,  1, 1,  1, -1, -1, -1]
        dy = [1, -1, 0, 1, -1,  1,  0, -1]
        -- Creo el nuevo tablero poniendo el numero en tablero anterior
        cellList = replaceAt (x, y) num (cells board)
        newBoard = Board cellList (minNum board) lastNum
        -- Calculo el siguiente  numero que voy a poner
        newNum = num + 1
        -- Busco el siguiente board llamando recursivamente con la matriz direccional
        maybeBoard = recursiveCall 0

        recursiveCall index
            -- Ya probé todas las direcciones
            | index == length dx = Empty
            -- Si no esta en rango o el elemento en la nueva posicion esta ocupado entonces no puedo poner el numero
            | not (inRange (newX, newY) cellList) || cellList!!newX!!newY /= 0 = recursiveCall (index + 1)
            -- Si el numero de ceros conectados es menor que los ceros que quedan entonces no es una solucion valida
            | canDisconnect (cells newBoard) x y && connected (cells newBoard) newX newY < lastNum - num = recursiveCall (index + 1)
            -- Cuando encuentre la primera solucion valida retorno
            | solution /= Empty = solution
            -- Si no he encontrado solucion pruebo con otra direccion
            | otherwise = recursiveCall (index + 1)
            where
                -- Calculo la nueva posicion
                (newX, newY) = (x + dx!!index, y +dy!!index)
                -- Llamo para buscar si poniendo el siguiente numero en la nueva posicion es solucion
                solution = fillR newBoard newNum (newX, newY) lastNum


------------------------------- function to get the column of a matrix -------------------------------
getColumn :: [[Integer]] -> Int -> [Integer] 
getColumn matrix y = map (\x -> x!!y) matrix

------------------------------- function to find cells in the board between min and max -------------------------------

findPairs :: [[Integer]] -> Integer-> Integer-> [(Int,Int)]
findPairs matrix max min = 
    [(x,y) | (x, rows) <-enumerate matrix, (y,value)<- enumerate rows, value < max, value >= min ]        
    where
        enumerate = zip [0..]

------------------------------- Make 0 some of the cells in the board. Recursive function -------------------------------

removeNum :: Board -> [(Int,(Int,Int))] -> Int -> Board
removeNum board randList n
    | n == 0 = board
    | otherwise = removeNum newBoard (rest) (n-1)
    where
        (init:rest) = (randList)
        (index,pos) = init
        newCells = replaceAt pos 0 (cells board)
        newBoard = Board newCells (minNum board) (maxNum board)

-- -------------------generate a list of random integers
randomList :: Int -> StdGen -> [Int]
randomList n = take n . unfoldr (Just . random)


-------------main function ----------------
-- generate :: Integer -> Integer -> Utils.Board
generate a b = do
    -------make the seed
    seed1 <- newStdGen

    ---------build the board with the specified form
    --------- mirrorBoard, cloudBoard and squareBoard
    ----------you pass the dimensions of the board and 
    randShape <- randomRIO (0,2) 
    let shapes = [(cloudBoard a b),(mirrorBoard a b),(squareBoard a b)]
    let sel = shapes!!randShape
    
    let blankBoard = (sel)

    -----------the initial position for 1 is generated (index start at 0)
    let blankList = findPairs (cells blankBoard) (1) (0)
    randIndex <- randomRIO (0,(length (blankList)-1))
    let initIndex = blankList!!(randIndex)

    -------fill the board with numbers
    let board = fill blankBoard initIndex

    -------get a list for the ocupated positions
    let listPos = findPairs (cells board) (maxNum board) (2)
    let n = length (listPos)

    -----------make a list of random indexes to match with listPos and shuffle it
    let list = map(\x -> (mod x n)) (randomList n seed1)
    let eliminationList = sort (zip (list) (listPos))

    -----------replace with 0 the number of cells passed
    let genBoard = removeNum board eliminationList (round (fromInteger (maxNum (board)) / 2))

    ------return
    --printBoard genBoard
    return (genBoard)