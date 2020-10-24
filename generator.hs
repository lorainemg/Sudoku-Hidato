import Data.List
import Data.Maybe
import System.Random

-- generate at least 2 types of board in blank
-- defining a standard way (hearted or rombus or cloud)
------------------------------- Board Definition -------------------------------
data Board = Board {
    cells :: [[Integer]],
    minNum :: Integer,
    maxNum :: Integer
} | Empty deriving (Show, Eq)

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

------------------------------- Prints the board -------------------------------
printBoard :: Board -> IO ()
printBoard Empty = putStrLn "Empty"
printBoard board = putStrLn (showBoard board)

------------------------------- Returns the board in a string -------------------------------
showBoard :: Board -> String
showBoard board = showCells cellList 0 0 (length cellList) (length line) maxSpace
    where
        cellList = cells board 
        line = head cellList
        maxSpace = length (show (maxNum board))

        
-------------------------- Returns the cells in a String --------------------------
showCells :: [[Integer]] -> Int -> Int-> Int-> Int -> Int -> String
showCells cellList pRow pCol endRow endCol maxSpace
    | pRow == endRow = ""
    | pCol == endCol = "\n" ++ showCells cellList (pRow + 1) 0 endRow (length line) maxSpace
    | otherwise = spaces ++ elem ++ showCells cellList pRow (pCol + 1) endRow endCol maxSpace
        where 
            line = cellList !! (pRow + 1)
            el =  cellList !! pRow !! pCol
            elem = if el == -1 then " " else show el
            spaces =  concat [" " | _ <- [0..(maxSpace - length elem)]]  


------------------------------- Replace an element in a matrix  -------------------------------
-- Recibe la posicion, el numero y la matriz y retorna la matriz con el elemento reemplazado
replaceAt :: Eq a => (Int, Int) -> a -> [[a]] -> [[a]]
replaceAt (x,y) num cellList = a1 ++ (newLine:b1)
    where
        -- encuentra la linea dividiendo por las filas
        (a1, line:b1) = splitAt x cellList
        -- encuentra la posicon dividiendo la linea en la columna 
        (a2, _:b2) = splitAt y line
        newLine = a2 ++ (num:b2)


----------------------- Decides if there is no num to put in the board -----------------------
-- Si todas las celdas alrededor son distintas de -1 entonces estan ocupadas y no se puede continuar

cantContinue :: Integer -> (Int, Int) -> ([Int], [Int]) -> [[Integer]] -> Bool
cantContinue num (x, y) (dx, dy) cellList =
        0 `notElem` [cellList!!(x+nX)!!(y+nY) | nX <- dx, nY <- dy, inRange (x+nX,y+nY) cellList]

------------------------ Returns if position is in range in a matrix  ------------------------
inRange :: (Int, Int) -> [[Integer]] -> Bool
inRange (x, y) matrix =
    x >= 0 && x < nRow && y >= 0 && y < nCol
    where
        nRow = length matrix
        nCol = length (matrix !! x)

------------------------------- Main function to fill a board -------------------------------
fill :: Board -> (Int,Int) -> Board
fill board pos = newBoard
    where
        num = minNum board
        -- se busca el ultimo numero que se debe escribir en el tablero
        lastNum = sum ([ 1 | x <- concat (cells board), x==0])
        -- se llama a la funcion recursiva para resolver el tablero
        newBoard = fillR board num pos lastNum

------------------------------- Recursive function to fill the board -------------------------------
fillR :: Board -> Integer -> (Int, Int) -> Integer -> Board
fillR board num (x, y) lastNum
    -- Si solo falta poner el ultimo numero entonces ya termino
    | num == lastNum = newBoard
    -- Si el numero de ceros conectados es menos que los ceros que quedan entonces no es una solucion valida
    | connected (cells board) x y < lastNum - num + 1 = Empty
    -- Si todas las casillas alrededor del numero estan ocupadas entonces no puedo poner un numero nuevo y retorno
    | cantContinue num (x, y) (dx, dy) (cells board) = Empty
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
            -- Cuando encuentre la primera solucion valida retorno
            | solution /= Empty = solution
            -- Si no he encontrado solucion pruebo con otra direccion
            | otherwise = recursiveCall (index + 1)
            where
                -- Calculo la nueva posicion
                (newX, newY) = (x + dx!!index, y +dy!!index)
                -- Llamo para buscar si poniendo el siguiente numero en la nueva posicion es solucion
                solution = fillR newBoard newNum (newX, newY) lastNum


connected: [[Integer]] -> Int -> Int -> Integer
connected matrix x y 
    | length matrix == 0 || matrix!!x!!y /=0 = 0
    | otherwise = ceros + (connected (take x matrix) nX1 nY1) + (connected (drop (x+1) matrix) nX2 nY2)
    where
        ceros = (cerosInRow (matrix!!x) y 1) + (cerosInRow (matrix!!x) y -1) - 1
        (nX1, nY1) = cerosAt matrix (x-1) y 
        (nX2, nY2) = cerosAt matrix (x+1) y
        cerosInRow :: [Integer] -> Int -> Int -> Integer
        cerosInRow row c i
            | c >= length row || c < 0| | row!!c /= 0 = 0
            | otherwise = 1 + (cerosAtLeft row (c+i) i)
        cerosAt :: [[Integer]] -> Int -> Int -> (Int, Int)
        cerosAt matrix r c
            | inRange (r, c) matrix && matrix!!r!!c == 0 = (r, c)
            | inRange (r, c+1) matrix && matrix!!r!!(c+1) == 0 = (r, c+1)
            | otherwise = (r, c-1) 

connected :: [[Integer]] -> Int -> Int -> Integer
connected matrix x y = connectedR matrix mask (x, y)
    where
        mask = [[False | yi <- [0..length (matrix!!xi) - 1]] | xi <- [0..length matrix - 1]]
        connectedR matrix mask (x, y)
            | matrix!!x!!y /= 0 = 0
            | otherwise = 1 + solution
            where 
                dx = [0, 0,  1, 1,  1, -1, -1, -1]
                dy = [1, -1, 0, 1, -1,  1,  0, -1]
                nMask = replaceAt (x, y) True mask
                ceros = [(x+(dx!!i), y + (dy!!i)) | i <-[0..length dx-1], inRange (x+(dx!!i), y+(dy!!i)) matrix, matrix!!(x+(dx!!i))!!(y+(dy!!i)) == 0, not (nMask!!(x+(dx!!i))!!(y+(dy!!i)))]
                newMask = createMask ceros nMask
                solution = sum(map (\x -> (connectedR matrix newMask x)) ceros)  
                createMask ceros mask =
                    if length ceros == 0 then mask else createMask nCeros nMask
                    where
                        nCeros = drop 1 ceros
                        nMask = replaceAt (head ceros) True mask


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
    printBoard genBoard