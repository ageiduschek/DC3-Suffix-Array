module SuffixArray (SuffixArray, GeneralizedSuffixArray, createSuffixArray, createGeneralizedSuffixArray, toBurrowsWheeler, fromBurrowsWheeler, lce) where

import Utility
import FischerHeun
--import Data.HashTable as HT
--import Control.Monad
import Data.Array.IO

type Length = Int
type StrNum = Index

type LCPInfo = [Length]

data SuffixRankings = SuffixRankings [Index] 
data GeneralizedSuffixRankings = GeneralizedSuffixRankings [(StrNum, Index)]

data StrChar = ActualChar Char | PseudoEOF Index

instance Eq StrChar where
    (PseudoEOF eof1) == (PseudoEOF eof2) = eof1 == eof2
    (ActualChar ch1) == (ActualChar ch2) = ch1 == ch2
    _ == _ = False

instance Ord StrChar where
    (PseudoEOF _) `compare` (ActualChar _) = LT
    (PseudoEOF eof1) `compare` (PseudoEOF eof2) = eof1 `compare` eof2
    (ActualChar ch1) `compare` (ActualChar ch2) = ch1 `compare` ch2
    _ `compare` _ = GT

-- A suffix array for a single string
data SuffixArray = SuffixArrayConstructor { inputStr :: [StrChar]  
                     , orderedSuffixes :: SuffixRankings  
                     , lcp :: LCPInfo
                     , lcpRMQ :: FischerHeun  
                     } 

-- A suffix array for many strings
data GeneralizedSuffixArray = GeneralizedSuffixArrayConstructor {  inputStrs :: [[StrChar]] 
                                                        , genOrderedSuffix :: GeneralizedSuffixRankings
                                                        , numInputStrs :: Int 
                                                        , strIndexToOverallIndex :: (StrNum, Index) -> Index 
                                                        , genLcp :: LCPInfo 
                                                        , genLcpRMQ :: FischerHeun  
                                                        } 

initialAlphabetSize :: Int
initialAlphabetSize = 256

createSuffixArray :: String -> IO SuffixArray
createSuffixArray str = do
    let str' = appendEOF str 0
    sa <- strToSuffixArray str' (length str') initialAlphabetSize
    let arr =  SuffixRankings sa
    let lcp = lCPInfo arr
    lcpRMQ <- fischerHeunRMQ lcp
    return $ SuffixArrayConstructor {inputStr = str', orderedSuffixes = arr, lcp = lcp, lcpRMQ = lcpRMQ}


-- Creates a generalized suffix array out of a list of strings
createGeneralizedSuffixArray :: [String] -> IO GeneralizedSuffixArray
createGeneralizedSuffixArray strs = do
    let inputStrs = zipWith (appendEOF) strs [0..]
    let (toIndividualStrAddr, toOverallAddr) = getToFromGeneralMaps inputStrs
    let str' = (concat inputStrs)
    sa <- strToSuffixArray str' (length str') initialAlphabetSize
    let arr =  GeneralizedSuffixRankings $ map toIndividualStrAddr sa
    let lcp = genLCPInfo arr
    lcpRMQ <- fischerHeunRMQ lcp
    return $ GeneralizedSuffixArrayConstructor {  inputStrs = inputStrs 
                                                , genOrderedSuffix = arr 
                                                , numInputStrs = length inputStrs
                                                , strIndexToOverallIndex = toOverallAddr  
                                                , genLcp = lcp 
                                                , genLcpRMQ = lcpRMQ  
                                            }

-- This performs the DC3 Algorithm
strToSuffixArray :: [StrChar] -> Int -> Int -> IO [Index]
strToSuffixArray [x] _ _ = do return [0]
strToSuffixArray str strlen alphabetSize = do
    let t1t2Order = getT1AndT2Ordering str strlen alphabetSize
    unsortedRanks <- unsort t1t2Order strlen
    t0Order <- getT0Ordering str strlen alphabetSize t1t2Order unsortedRanks
    return $ mergeT0WithRest str t0Order t1t2Order unsortedRanks

getT1AndT2Ordering :: [StrChar] -> Int -> Int -> [Index]
getT1AndT2Ordering str strlen alphabetSize = undefined

{-
    private int[] getT1AndT2Ordering(int[] input, int alphabetSize){
        int [] doubledInput = shiftAndDouble(input);

        int [] tokenOrderWithRepeats = radixSort(doubledInput, 3, alphabetSize);
        int [] indicesWithRepeatsRemoved = indicesWithoutRepeats(doubledInput, tokenOrderWithRepeats);
        
        int [] tokenOrder = new int[tokenOrderWithRepeats.length];
        for(int i = 0; i < tokenOrderWithRepeats.length; i++) {
            tokenOrder[tokenOrderWithRepeats[i]] = indicesWithRepeatsRemoved[i];
        }

        int newAlphabetSize = indicesWithRepeatsRemoved[indicesWithRepeatsRemoved.length -1] + 1;
        int [] twoThirdsSuffixArray = buildSuffixArray(tokenOrder, newAlphabetSize);

        return mapDoubledArrayTokensToMaster(twoThirdsSuffixArray);
    }
-}

unsort :: [Index] -> Int -> IO [Index]
unsort t1t2Order len = undefined

{-
    private int[] unsort(int [] sortedIndices, int mappingSpace) {
        int [] result = new int[mappingSpace];
        for(int i=0; i < sortedIndices.length; i++) {
            result[sortedIndices[i]] = i;
        }
        return result;
    }
-}


mapIOArr :: (Int -> Int) -> Int -> Int -> IOArray Int Int -> IO ( IOArray Int Int ) 
mapIOArr f i bound arr
    | i == bound = return arr
    | otherwise = 
       do
        val <- readArray arr i 
        let val' = f val
        writeArray arr i val' 
        mapIOArr f (i + 1) bound arr 

getT0Ordering :: [StrChar] -> Int -> Int -> [Index] -> [Index] ->  IO [Index]
getT0Ordering str strlen alphabetSize t1t2Order unsortedRanks = do
    let tokens = getT0Tokens str unsortedRanks -- tokens is int []
    sortedIndices <- radixSort tokens 2 $ max alphabetSize strlen
    mapT0ToMaster sortedIndices

{-
    private int[] getT0Ordering(int[] input, int [] t1t2Order, int [] unsortedRanks, int alphabetSize){        
        int [] tokens = getT0Tokens(input, unsortedRanks);
    
        int[] sortedIndices = radixSort(tokens, 2, Math.max(alphabetSize, input.length));
        int [] master = mapT0ToMaster(sortedIndices);
        return mapT0ToMaster(sortedIndices);
    }
-}

getT0Tokens :: [StrChar] -> [Index] -> [Int]
getT0Tokens str unsortedRanked = undefined
{-
    private int [] getT0Tokens(int[] input, int[] unsortedRanks) {
        int numTokens = input.length/3 + (input.length %3 == 0 ? 0 : 1);
        int [] tokens = new int [numTokens * 2];
        for(int tokenNum = 0; tokenNum < numTokens; tokenNum++) {
            int index = tokenNum * 3;
            int ch = input[index];
            tokens[tokenNum * 2] = ch;
            tokens[tokenNum * 2 + 1] = (index + 1) == input.length? -1 : unsortedRanks[index + 1];
        }

        return tokens;
    }
-}

radixSort :: [Int] -> Int -> Int -> IO [Index]
radixSort tokens tokenSize alphabetSize = undefined
{-
    private int[] radixSort(int [] tokens, int tokenSize, int alphabetSize){

        int [] sortedIndices = null;
        for(int round = tokenSize - 1; round >= 0; round--) {
            int [] partiallySortedIndices = sortedIndices;
            sortedIndices = new int[tokens.length/tokenSize];
            ArrayList<Queue<Integer>> buckets = initBuckets(alphabetSize + 1);
            for(int i = 0; i < tokens.length/tokenSize; i++) {
                int tokenIndex = partiallySortedIndices == null ? i : partiallySortedIndices[i];
                int ch = tokens[tokenIndex * tokenSize + round];
                buckets.get(ch + 1).add(tokenIndex);
            }
            int counter = 0;
            for(int ch = 0; ch < alphabetSize + 1; ch++) {
                while (!buckets.get(ch).isEmpty()) {
                    sortedIndices[counter] = buckets.get(ch).poll();
                    counter++;
                }      
            }

        }
        return sortedIndices;
    }
-}

mapT0ToMaster :: [Index] -> IO [Index]
mapT0ToMaster sortedIndices = undefined
{-
    private int [] mapT0ToMaster(int [] t0Indices) {
        int [] masterIndices = new int[t0Indices.length];
        for(int i=0; i < t0Indices.length; i++) {
            masterIndices[i] = t0IndexToMasterIndex(t0Indices[i]);
        }
        return masterIndices;
    }
-}

mergeT0WithRest :: [StrChar] -> [Index] -> [Index] -> [Index] -> [Index]
mergeT0WithRest str t0Order t1t2Order unsortedRanks = undefined

{-
    private int[] mergeT0WithRest(int[] input, int [] t0Order, int [] t1t2Order, int [] unsortedRanks) {
        int [] suffixArray = new int[input.length];
        int i = 0, j = 0;
        while (i < t0Order.length && j < t1t2Order.length) {           
            if(suffixCompare(i, j, input, t0Order, t1t2Order, unsortedRanks)) { //Returns true if i comes first
                suffixArray[i + j] = t0Order[i];
                i++;
            } else {
                suffixArray[i + j] = t1t2Order[j];
                j++;
            }
        }
        for(; i < t0Order.length; i++) suffixArray[i + j] = t0Order[i];
        for(; j < t1t2Order.length; j++) suffixArray[i + j] = t1t2Order[j];

        return suffixArray;
    }
-}



appendEOF :: String -> Index -> [StrChar]
appendEOF [] i = (PseudoEOF i):[]
appendEOF (x:xs) i = (ActualChar x):(appendEOF xs i)

getToFromGeneralMaps :: [[StrChar]] -> (Index -> (StrNum, Index), (StrNum, Index) -> Index)
getToFromGeneralMaps strs = (toIndividualStrAddr, toOverallAddr)
    where 
        strsWithIndices = zip [0..] strs --[(0, "foo$"), (1, "bar$"), (2, "a$")]
        strsOfIndices = map (\(i, str) -> replicate (length str) i) strsWithIndices -- [[0, 0, 0, 0], [1, 1, 1, 1], [2, 2]]
        individualStrAddresses = concat $ map (\x -> zip x [0..]) strsOfIndices -- [(0, 0), (0, 1), (0, 2), (0, 3), (1, 0), (1, 1), (1, 2), ...]
        toIndividualStrAddr i =  individualStrAddresses !! i
        toOverallAddr (strNum, i) = i + (map length strsOfIndices) !! strNum


-- The following two functions will need to implement the algorithm in http://www.cs.iastate.edu/~cs548/references/linear_lcp.pdf
lCPInfo :: SuffixRankings -> LCPInfo
lCPInfo indices = undefined

genLCPInfo :: GeneralizedSuffixRankings -> LCPInfo
genLCPInfo indices = undefined

-- Takes a string and a user provided eof character that doesn't appear anywhere in the string
toBurrowsWheeler :: String -> Char -> String
toBurrowsWheeler str eof = undefined

-- Takes a string and a user provided eof character that doesn't appear anywhere in the string
fromBurrowsWheeler :: String -> Char -> String
fromBurrowsWheeler str eof = undefined

-- Longest common extension. Array of indices must match number of strings in GeneralizedSuffixArray
lce :: GeneralizedSuffixArray -> [Index] -> Int
lce sa indices = if length indices /= numInputStrs sa 
                    then error "must provide same number of indices as there are in the GeneralizedSuffixArray" 
                    else (genLcpRMQ sa) (minimum addresses) (maximum addresses)
        where 
            addresses = map (strIndexToOverallIndex sa) (zip [0..] indices)
