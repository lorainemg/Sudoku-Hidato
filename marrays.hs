import Control.Monad.ST
import Data.Array.ST

-- buildPair :: (MArray a e i, Ix i) => Int -> Int -> a e i
--buildPair :: [Int] -> ST s (STArray s Int Bool)
buildPair list = newListArray (1, end) newList 
    where
        newList = [False | x <- list]
        end = length list
        

-- main = getElems (buildPair 1 10)
testing matrix = a!!0
    where
        a = [buildPair x | x <- matrix]

getElem matrix = readArray arr 1
    where 
        arr = testing matrix

-- main = print $ runST (getElem [[1,2,4,5], [1,2,4,5]])
