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
                    let (range1Start, range2Start) = (startIndex, prevRangeLength)

                    leftMinIndex <- st' !-!-! (range1Start, k-1)
                    rightMinIndex <- st' !-!-! (range2Start, k-1)
                    overallMinIndex <- getOverallMin elements leftMinIndex rightMinIndex
                    st' !-!-!= (startIndex, k, overallMinIndex)
                    return () 

        )
    return $ rmq $ ST {kArray=kArr, sparseTable = st, elems = elements}


{-
    public SparseTableRMQ(float[] elems) {
        this.elems = elems;

        kArray = createKArray(elems.length);
        
        int maxK = kArray[elems.length];
        
        sparseTable = new int [elems.length][maxK + 1];

        for(int k = 0; k < maxK + 1; k++){
            for (int startIndex=0; startIndex < elems.length - rangeLength + 1; startIndex++) {
                int rangeLength = powerOfTwo(k);
                if(k == 0){
                    sparseTable[startIndex][k] = startIndex;
                } else {
                    int prevRangeLength = powerOfTwo(k-1);
                    int range1Start = startIndex;
                    int range2Start = startIndex + prevRangeLength;

                    int leftMinIndex = sparseTable[range1Start][k-1];
                    int rightMinIndex = sparseTable[range2Start][k-1];
                    int overallMinIndex = elems[leftMinIndex] < elems[rightMinIndex] ? leftMinIndex : rightMinIndex;
                    sparseTable[startIndex][k] = overallMinIndex;
                }
                
            }
        }
    }
-}

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


{-
    public int rmq(int i, int j) {
        int range = j - i + 1;
        int k = kArray[range];
        int rangeLength = powerOfTwo(k); //2^k
        int range1Start = i;
        int range2Start = j - rangeLength + 1;

        int leftMinIndex = sparseTable[range1Start][k];
        int rightMinIndex = sparseTable[range2Start][k];
        return elems[leftMinIndex] < elems[rightMinIndex] ? leftMinIndex : rightMinIndex;
    }
-}

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
        let (k', numRepeats') = if numRepeats == (twoToThe k) then (k + 1, 0) else (k, numRepeats + 1)
        writeArray arr i k' 
        createKArray' arr (i + 1) len k' numRepeats'

{-
    private int powerOfTwo(int k) {
        return 1 << k;
    }

    private int[] createKArray(int arrayLength) {
        int[] kArray = new int[arrayLength + 1];

        int k = 0;
        int numRepeats = 0;
        for (int i=1; i < arrayLength + 1; i++){
            if(numRepeats == powerOfTwo(k)) {
                k++;
                numRepeats = 0;
            } 
            kArray[i] = k;
            numRepeats++;
        }

        return kArray;

    }
-}
