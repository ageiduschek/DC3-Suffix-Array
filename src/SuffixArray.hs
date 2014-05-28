module SuffixArray (SuffixArray, GeneralizedSuffixArray, createSuffixArray, createGeneralizedSuffixArray, toBurrowsWheeler, fromBurrowsWheeler, lce) where

import FischerHeun

type Length = Int
type StrNum = Index

type LCPInfo = [Length]

data SuffixRankings = SingleSuffixRankings [Index] | GeneralizedSuffixRankings [(StrNum, Index)]

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
data SuffixArray = SuffixArray{ inputStr :: [StrChar]  
                     , orderedSuffixes :: SuffixRankings  
                     , lcp :: LCPInfo 
                     , lcpRMQ :: FischerHeun  
                     } 

-- A suffix array for many strings
data GeneralizedSuffixArray = GeneralizedSuffixArray {} -- TODO: Implement

createSuffixArray :: String -> SuffixArray
createSuffixArray str = SuffixArray {inputStr = str', orderedSuffixes = arr, lcp = lcp, lcpRMQ = lcpRMQ}
    where 
        str' = (appendEOF str)
        arr =  SingleSuffixRankings $ strToSuffixArray str' 256
        lcp = generateLCPInfo arr
        lcpRMQ = fischerHeunRMQ lcp


-- Creates a generalized suffix array out of a list of strings
createGeneralizedSuffixArray :: [String] -> GeneralizedSuffixArray
createGeneralizedSuffixArray strs = undefined
    -- toGeneralized strs $ strToSuffixArray $ concatenateInputStrs strs

concatenateInputStrs :: [String] -> [StrChar]
concatenateInputStrs strs = undefined

strToSuffixArray :: [StrChar] -> Int -> [Index]
strToSuffixArray str alphabetSize = undefined

appendEOF :: String -> [StrChar]
appendEOF str = undefined

toGeneralized :: [String] -> [Index] -> [(StrNum, Index)]
toGeneralized strs sa = undefined

generateLCPInfo :: SuffixRankings -> LCPInfo
generateLCPInfo orderedSuffixes = undefined


-- Takes a string and a user provided eof character that doesn't appear anywhere in the string
toBurrowsWheeler :: String -> Char -> String
toBurrowsWheeler str eof = undefined

-- Takes a string and a user provided eof character that doesn't appear anywhere in the string
fromBurrowsWheeler :: String -> Char -> String
fromBurrowsWheeler str eof = undefined

-- Longest common extension. Array of indices must match number of strings in GeneralizedSuffixArray
lce :: GeneralizedSuffixArray -> [Index] -> Int
lce sa indices = undefined
