module SuffixArray (SuffixArray, GeneralizedSuffixArray, createSuffixArray, createGeneralizedSuffixArray, toBurrowsWheeler, fromBurrowsWheeler, lce) where

import FischerHeun

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

createSuffixArray :: String -> SuffixArray
createSuffixArray str = SuffixArrayConstructor {inputStr = str', orderedSuffixes = arr, lcp = lcp, lcpRMQ = lcpRMQ}
    where 
        str' = appendEOF str 0
        arr =  SuffixRankings $ strToSuffixArray str' initialAlphabetSize
        lcp = lCPInfo arr
        lcpRMQ = fischerHeunRMQ lcp


-- Creates a generalized suffix array out of a list of strings
createGeneralizedSuffixArray :: [String] -> GeneralizedSuffixArray
createGeneralizedSuffixArray strs = GeneralizedSuffixArrayConstructor {  inputStrs = inputStrs 
                                                                        , genOrderedSuffix = arr 
                                                                        , numInputStrs = length inputStrs
                                                                        , strIndexToOverallIndex = toOverallAddr  
                                                                        , genLcp = lcp 
                                                                        , genLcpRMQ = lcpRMQ  
                                                                    }
     where 
        inputStrs = zipWith (appendEOF) strs [0..]
        (toIndividualStrAddr, toOverallAddr) = getToFromGeneralMaps inputStrs
        arr =  GeneralizedSuffixRankings $ map toIndividualStrAddr $ strToSuffixArray (concat inputStrs) initialAlphabetSize
        lcp = genLCPInfo arr
        lcpRMQ = fischerHeunRMQ lcp


-- This performs the DC3 Algorithm
strToSuffixArray :: [StrChar] -> Int -> [Index]
strToSuffixArray str alphabetSize = undefined

appendEOF :: String -> Index -> [StrChar]
appendEOF [] index = (PseudoEOF index):[]
appendEOF (x:xs) index = (ActualChar x):(appendEOF xs index)

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
