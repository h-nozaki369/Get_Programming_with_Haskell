module Main (main) where

import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ do
    let end = length vals - 1
    myArray <- newArray (0,end) 0
    forM_ [0 .. end] $ \i -> do
        let val = vals !! i
        writeArray myArray i val
    return myArray

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do
    stArray <- thaw myArray
    let end = (snd . bounds) myArray
    forM_ [1 .. end] $ \i -> do
        forM_ [0 .. (end - i)] $ \j -> do
            val <- readArray stArray j
            nextVal <- readArray stArray (j + 1)
            let outOfOrder = val > nextVal
            when outOfOrder $ do
                writeArray stArray j nextVal
                writeArray stArray (j + 1) val
    return stArray

myData :: UArray Int Int
myData = listArray (0,5) [7,6,4,8,10,2]

-- Q42-1
crossover :: Int -> (UArray Int Int, UArray Int Int) -> UArray Int Int
crossover limit (array1, array2) = runSTUArray $ do
    stArray1 <- thaw array1
    let end = (snd . bounds) array1
    forM_ [limit .. end] $ \i -> do
        let val = array2 ! i
        writeArray stArray1 i val
    return stArray1

-- Q42-2
replaceZeros :: UArray Int Int -> UArray Int Int
replaceZeros array1 = runSTUArray $ do
    stArray1 <- thaw array1
    let (lb, ub) = bounds array1
    forM_ [lb .. ub] $ \i -> do
        val <- readArray stArray1 i
        when (val == 0) $ do
            writeArray stArray1 i (-1)
    return stArray1

main :: IO ()
main = print "st-lesson"
