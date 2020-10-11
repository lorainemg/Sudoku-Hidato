data Board = Board {
    cells :: [[Integer]],
    maxNum :: Int, 
    minNum :: Int
} deriving Show

printBoard board =
    putStr (printCells cellList 0 0 (length cellList) (length line) maxSpace)
        where
            cellList = cells board 
            line = head cellList
            maxSpace = length (show (maxNum board))

printCells :: [[Integer]] -> Int -> Int-> Int-> Int -> Int -> String
printCells cellList pRow pCol endRow endCol maxSpace
    | pRow == endRow = ""
    | pCol == endCol = "\n" ++ printCells cellList (pRow + 1) 0 endRow (length line) maxSpace
    | otherwise = spaces ++ elem ++ printCells cellList pRow (pCol + 1) endRow endCol maxSpace
        where 
            line = cellList !! (pRow + 1)
            el =  cellList !! pRow !! pCol
            elem = if el == -1 then " " else show el
            spaces =  concat [" " | _ <- [0..(maxSpace - length elem)]]  


sample = Board [
    [0, 33, 15, 0, 0],
    [0, 0, 24, 22, 0],
    [0, 0, 0, 21, 0, 0],
    [0, 26, 0, 13, 40, 11],
    [27, 0, 0, 0, 9, 0, 1],
    [-1, -1, 0, 0, 18, 0, 0],
    [-1, -1, -1, -1, 0, 7, 0, 0],
    [-1, -1, -1, -1, -1, -1, 5, 0]]
    40 1
