module SparseTable (SparseTable, sparseTableRMQ) where

import Utility
import Data.Array.IO

type SparseTable = Index -> Index -> IO Index

data ST = ST { kArray :: IOArray Index Int   -- Maybe make this an IOArray?
                , sparseTable :: IOArray Index (IOArray Index Index)  
                , elems :: IOArray Index Int
            }  

    --private int [] kArray; //Array of length n+1
    --private int [][] sparseTable;
    --private float [] elems;

sparseTableRMQ :: [Int] -> IO SparseTable
sparseTableRMQ a = undefined

{-
    public SparseTableRMQ(float[] elems) {
        this.elems = elems;

        kArray = createKArray(elems.length);
        
        int maxK = kArray[elems.length];
        
        sparseTable = new int [elems.length][maxK + 1];

        for(int k = 0; k < maxK + 1; k++){
            int rangeLength = powerOfTwo(k);
            for (int startIndex=0; startIndex < elems.length - rangeLength + 1; startIndex++) {
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
    let range = j - i + 1
    k <- (kArray st) !-! range
    let rangeLength = twoToThe k
    let (range1Start, range2Start)= (i, j - rangeLength + 1)
    leftMinIndex <- (sparseTable st) !-!-! (range1Start, k)
    rightMinIndex <- (sparseTable st) !-!-! (range2Start, k)
    getOverallMin st leftMinIndex rightMinIndex


getOverallMin :: ST -> Index -> Index -> IO Index
getOverallMin st leftMinIndex rightMinIndex = do
    leftValue <- (elems st) !-! leftMinIndex
    rightValue <- (elems st) !-! rightMinIndex
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
