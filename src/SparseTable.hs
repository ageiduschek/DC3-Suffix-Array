module SparseTable (SparseTable, sparseTableRMQ) where

import Utility
import Data.Array.IO

type SparseTable = Index -> Index -> IO Index

data ST = ST { kArray :: IOArray Index Int   -- Maybe make this an IOArray?
                , sparseTable :: IOArray (Index, Index) Int 
                , elems :: IOArray Index Int
            }  

    --private int [] kArray; //Array of length n+1
    --private int [][] sparseTable;
    --private float [] elems;

sparseTableRMQ :: [Int] -> IO SparseTable
sparseTableRMQ a = do
    let elemsLength = length a
    kArr <- createKArray elemsLength
    maxK <- kArr !-! elemsLength
    st <- new2DArray elemsLength (maxK + 1) 0
    elements <- (newListArray (0, elemsLength -1) a)

    loopOuter st (0, maxK + 1) (\i -> (0, elemsLength - (twoToThe i) + 1)) (\st' (k, startIndex) -> do
            if k == 0
                then
                    st' !-!-!= (startIndex, k, startIndex) 
                else do
                    let prevRangeLength = twoToThe (k-1)
                    let (range1Start, range2Start) = (startIndex, startIndex + prevRangeLength)

                    leftMinIndex <- st' !-!-! (range1Start, k-1)
                    rightMinIndex <- st' !-!-! (range2Start, k-1)
                    overallMinIndex <- getOverallMin elements leftMinIndex rightMinIndex
                    st' !-!-!= (startIndex, k, overallMinIndex)
                    return () 

        )
    return $ rmq $ ST {kArray=kArr, sparseTable = st, elems = elements}

-- FOR TESTING ONLY
--getSparseTable :: [Int] -> IO ST
--getSparseTable a = do
--    let elemsLength = length a
--    kArr <- createKArray elemsLength
--    maxK <- kArr !-! elemsLength
--    st <- new2DArray elemsLength (maxK + 1) 0
--    elements <- (newListArray (0, elemsLength -1) a)

--    loopOuter st (0, maxK + 1) (\i -> (0, elemsLength - (twoToThe i) + 1)) (\st' (k, startIndex) -> do
--            if k == 0
--                then
--                    st' !-!-!= (startIndex, k, startIndex) 
--                else do
--                    let prevRangeLength = twoToThe (k-1)
--                    let (range1Start, range2Start) = (startIndex, startIndex + prevRangeLength)

--                    leftMinIndex <- st' !-!-! (range1Start, k-1)
--                    rightMinIndex <- st' !-!-! (range2Start, k-1)
--                    overallMinIndex <- getOverallMin elements leftMinIndex rightMinIndex
--                    st' !-!-!= (startIndex, k, overallMinIndex)
--                    return () 

--        )
--    return $ ST {kArray=kArr, sparseTable = st, elems = elements}

rmq :: ST -> Index -> Index -> IO Index
rmq st i  j = do
    let r = j - i + 1
    k <- (kArray st) !-! r
    let rangeLength = twoToThe k
    let (range1Start, range2Start)= (i, j - rangeLength + 1)
    leftMinIndex <- (sparseTable st) !-!-! (range1Start, k)
    rightMinIndex <- (sparseTable st) !-!-! (range2Start, k)
    getOverallMin (elems st) leftMinIndex rightMinIndex


getOverallMin :: IOArray Index Int -> Index -> Index -> IO Index
getOverallMin elements leftMinIndex rightMinIndex = do
    leftValue <- elements !-! leftMinIndex
    rightValue <- elements !-! rightMinIndex
    return $ if leftValue < rightValue then leftMinIndex else rightMinIndex

twoToThe :: Int -> Int
twoToThe k = 2 ^ k

createKArray :: Int -> IO (IOArray Index Int)
createKArray arrayLength = do
    arr <- newArray (0, arrayLength) 0
    createKArray' arr 1 (arrayLength + 1) 0 0

createKArray' :: (IOArray Index Int) -> Index -> Index -> Int -> Int -> IO (IOArray Index Int)
createKArray' arr i len k numRepeats
    | i == len = return arr
    | otherwise = do
        let (k', numRepeats') = if numRepeats == (twoToThe k) then (k + 1, 1) else (k, numRepeats + 1)
        writeArray arr i k' 
        createKArray' arr (i + 1) len k' numRepeats'
