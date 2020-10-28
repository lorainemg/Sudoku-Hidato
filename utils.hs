module Utils where
import Data.Set (Set)
import qualified Data.Set as Set


-- generate at least 2 types of board in blank
-- defining a standard way (hearted or rombus or cloud)
------------------------------- Board Definition -------------------------------
data Board = Board {
    cells :: [[Integer]],
    minNum :: Integer,
    maxNum :: Integer
} | Empty deriving (Show, Eq)

------------------------ Returns if position is in range in a matrix  ------------------------
inRange :: (Int, Int) -> [[Integer]] -> Bool
inRange (x, y) matrix =
    x >= 0 && x < nRow && y >= 0 && y < nCol
    where
        nRow = length matrix
        nCol = length (matrix !! x)


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


----------------------- Calculates the amount of zeros in the matrix -----------------------
calculateZeros :: [[Integer]] -> Integer
calculateZeros matrix = sum ([ 1 | x <- concat matrix, x==0])

----------------------- Decides if there is no num to put in the board -----------------------
-- Si todas las celdas alrededor son distintas de -1 entonces estan ocupadas y no se puede continuar
cantContinue :: (Int, Int) -> ([Int], [Int]) -> [[Integer]] -> Bool
cantContinue (x, y) (dx, dy) cellList =
        0 `notElem` [cellList!!(x+nX)!!(y+nY) | nX <- dx, nY <- dy, inRange (x+nX,y+nY) cellList]


----------------------- Calculates all the zeros connected in the matrix -----------------------
connected :: [[Integer]] -> Int -> Int -> Integer
connected matrix x y
    | matrix!!x!!y /= 0 = 0
    | otherwise = findFinalRes (Set.fromList [(x, y)])
    where
    -- Se itera sobre el conjunto de los ceros hasta que no haya ningun cambio
    findFinalRes :: Set (Int, Int) -> Integer
    findFinalRes zeros
        -- Si no hubo ningun cambio se retorna la cantidad de ceros
        | zeros == nZeros = toInteger (Set.size nZeros)
        -- En otro caso, se itera sobre el nuevo conjunto con las posiciones de los ceros hasta que no haya ningun cambio
        | otherwise = findFinalRes nZeros
        where
            nZeros = neighbors zeros
            -- Retorna todos los vecinos de la lista de ceros que tambien sean ceros
            neighbors :: Set (Int, Int) -> Set (Int, Int)
            neighbors zeros
                | Set.size zeros == 0 = Set.empty
                | otherwise = Set.union nZeros (neighbors popZeros)
                where
                    -- Arrays direccionales
                    dx = [0, 0, 0,  1, 1,  1, -1, -1, -1]
                    dy = [0, 1, -1, 0, 1, -1,  1,  0, -1]
                    (x, y) = Set.findMin zeros
                    popZeros = Set.deleteMin zeros
                    nZeros = Set.fromList [(x+dx!!i,y+dy!!i) | i <- [0..(length dx)-1], inRange (x+dx!!i, y+dy!!i) matrix, matrix!!(x+dx!!i)!!(y+dy!!i)==0]


canDisconnect :: [[Integer]] -> Int -> Int -> Bool
canDisconnect matrix x y
    | length zeros <= 1 = False
    | inRange (up, y) matrix && inRange (x, right) matrix && inRange (up, right) matrix && intercept up right = True
    | inRange (up, y) matrix && inRange (x, left) matrix && inRange (up, left) matrix && intercept up left = True
    | inRange (down, y) matrix && inRange (x, left) matrix && inRange (down, left) matrix && intercept down left = True
    | inRange (down, y) matrix && inRange (x, right) matrix && inRange (down, right) matrix && intercept down right = True
    | inRange (x,right) matrix && inRange (x,left) matrix && separatesC = True
    | inRange (down,y) matrix && inRange (up,y) matrix && separatesR = True
    | otherwise = False
    where
        up = x - 1
        down = x + 1
        left = y - 1
        right = y + 1
        intercept nX nY = matrix!!x!!nY /= 0 && matrix!!nX!!y /= 0 && matrix!!nX!!nY == 0
        separatesC = ((not (inRange (up,y) matrix)) || matrix!!up!!y /= 0)  &&
                     ((not (inRange (down,y) matrix)) || matrix!!down!!y /= 0) &&
                     ((inRange (up,left) matrix && matrix!!up!!left == 0) ||
                      (inRange (down,left) matrix && matrix!!down!!left == 0) || matrix!!x!!left == 0) &&
                     ((inRange (up,right) matrix && matrix!!up!!right == 0) ||
                      (inRange (down,right) matrix && matrix!!down!!right == 0) || matrix!!x!!right == 0)
        separatesR = ((not (inRange (x,left) matrix)) || matrix!!x!!left /= 0) &&
                     ((not (inRange (x,right) matrix)) ||  matrix!!x!!right /= 0) &&
                    ((inRange (up,left) matrix  && matrix!!up!!left == 0 ) ||
                     (inRange (up,right) matrix && matrix!!up!!right == 0) || matrix!!up!!y == 0) &&
                    ((inRange (down, left) matrix && matrix!!down!!left == 0 ) ||
                     (inRange (down, right) matrix && matrix!!down!!right == 0) || matrix!!down!!y == 0)
        -- Arrays direccionales
        dx = [0, 0, 0,  1, 1,  1, -1, -1, -1]
        dy = [0, 1, -1, 0, 1, -1,  1,  0, -1]
        zeros = filter (\x -> x==0) [matrix!!(x+dx!!i)!!(y+dy!!i) | i <- [0..length dx-1], inRange (x+dx!!i,y+dy!!i) matrix]