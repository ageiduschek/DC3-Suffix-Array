module SuffixArray (SuffixArray, GeneralizedSuffixArray, createSuffixArray, createGeneralizedSuffixArray, toBurrowsWheeler, fromBurrowsWheeler, lce, printSuffixes) where

import Utility
import FischerHeun
--import Data.HashTable as HT
--import Control.Monad
import Control.Applicative
import Data.Array.IO
import Data.List

type Length = Int
type StrNum = Index

type LCPInfo = IOArray Index Length

type SuffixRankings = [Index] 
type GeneralizedSuffixRankings = [(StrNum, Index)]


data StrChar = ActualChar Char | PseudoEOF Index deriving Show

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
                                                        , strIndexToSAIndex :: (StrNum, Index) -> IO Index 
                                                        , genLcp :: LCPInfo 
                                                        , genLcpRMQ :: FischerHeun  
                                                        } 


initialAlphabetSize :: Int
initialAlphabetSize = 256

createSuffixArray :: String -> IO SuffixArray
createSuffixArray str = do
    let str' = appendEOF str 0
    sa <- strToSuffixArray str' (length str') initialAlphabetSize 1
    lCP <- lCPInfo sa str'
    lcpList <- getElems lCP
    lCPRMQ <- fischerHeunRMQ lcpList
    return $ SuffixArrayConstructor {inputStr = str', orderedSuffixes = sa, lcp = lCP, lcpRMQ = lCPRMQ}

printSuffixes :: SuffixArray -> IO ()
printSuffixes (SuffixArrayConstructor _ [] _ _) = return ()
printSuffixes (SuffixArrayConstructor str (i:os) x y) = do
    printStrCharString (drop i str)
    printSuffixes (SuffixArrayConstructor str os x y)

printStrCharString :: [StrChar] -> IO ()
printStrCharString []                       = putStrLn ""
printStrCharString ((ActualChar x):strChar) = do
    putStr (x:"")
    printStrCharString strChar
printStrCharString ((PseudoEOF _):strChar)  = printStrCharString strChar


-- Creates a generalized suffix array out of a list of strings
createGeneralizedSuffixArray :: [String] -> IO GeneralizedSuffixArray
createGeneralizedSuffixArray strs = do
    let inputStrings = zipWith (appendEOF) strs [0..]
    let str' = (concat inputStrings)
    sa <- strToSuffixArray str' (length str') initialAlphabetSize (length inputStrings)
    (toIndividualStrAddr, strIndexToSuffixArrayIndex) <- getToFromGeneralMaps inputStrings sa
    arr <- mapM toIndividualStrAddr sa
    lCP <- lCPInfo sa str' --genLCPInfo arr inputStrings
    lcpList <- getElems lCP
    lCPRMQ <- fischerHeunRMQ lcpList
    return $ GeneralizedSuffixArrayConstructor {  inputStrs = inputStrings 
                                                , genOrderedSuffix = arr 
                                                , numInputStrs = length inputStrings
                                                , strIndexToSAIndex = strIndexToSuffixArrayIndex  
                                                , genLcp = lCP 
                                                , genLcpRMQ = lCPRMQ  
                                            }

-- This performs the DC3 Algorithm
strToSuffixArray :: [StrChar] -> Int -> Int -> Int -> IO [Index]
strToSuffixArray [_] _ _ _ = do return [0]
strToSuffixArray str strlen alphabetSize numEOFs = do
    t1t2Order <- getT1AndT2Ordering str strlen alphabetSize numEOFs
    unsortedRanks <- unsort t1t2Order strlen
    t0Order <- getT0Ordering str strlen alphabetSize numEOFs unsortedRanks
    mergeT0WithRest str strlen t0Order t1t2Order unsortedRanks

getT1AndT2Ordering :: [StrChar] -> Int -> Int -> Int -> IO [Index]
getT1AndT2Ordering str strlen alphabetSize numEOFs = do
    let (doubledInput, doubledInputLen) = shiftAndDouble str strlen
    tokenOrderWithRepeats <- radixSort doubledInput 3 alphabetSize numEOFs
    (indicesWithRepeatsRemoved, newAlphabetSize) <- indicesWithoutRepeats doubledInput doubledInputLen tokenOrderWithRepeats
    tokensInNewAlphabet <- translateToNewAlphabet tokenOrderWithRepeats indicesWithRepeatsRemoved
    twoThirdsSuffixArray <- strToSuffixArray tokensInNewAlphabet (length tokensInNewAlphabet) newAlphabetSize newAlphabetSize
    return $ mapDoubledArrayTokensToMaster twoThirdsSuffixArray

shiftAndDouble :: [StrChar] -> Int -> ([StrChar], Int)
shiftAndDouble input len = (shiftedAndDoubledStr, newStringLen)
    where 
        shiftedOnce = drop 1 input
        shiftedTwice = drop 2 input
        padsNeeded l = (3 - (l `mod` 3)) `mod` 3
        (firstHalfPads, secondHalfPads) = (padsNeeded (len - 1), padsNeeded (len - 2))
        (firstPadsArr, secondPadsArr) = (replicate firstHalfPads (PseudoEOF 0),  replicate secondHalfPads (PseudoEOF 0))
        shiftedAndDoubledStr = shiftedOnce ++ (firstPadsArr ++ (shiftedTwice ++ secondPadsArr))
        newStringLen = (2 * len) - 3 + firstHalfPads + secondHalfPads

indicesWithoutRepeats :: [StrChar] -> Int -> [Index] -> IO ([Index], Int)
indicesWithoutRepeats doubledInput doubledInputLen tokenOrderWithRepeats = do
    let numTokens = quot doubledInputLen 3
    ioDoubledInputTokens <- newListArray (0, numTokens - 1) $ strToDC3Tokens doubledInput
    (revResult, newAlphabetSize) <- indicesWithoutRepeatsHelper ioDoubledInputTokens tokenOrderWithRepeats [0] 0 1 numTokens    --- TODO: Not totally sure numTokens is the right bound
    return $ (reverse revResult, newAlphabetSize)


indicesWithoutRepeatsHelper :: IOArray Index [StrChar] -> [Index] -> [Index] -> Int -> Index -> Index -> IO ([Index], Int)
indicesWithoutRepeatsHelper ioDoubledInputTokens tokenOrderWithRepeats revResult rank i bound
    | i == bound = return (revResult, rank + 1)
    | otherwise = do
        let (tokenIndex1, tokenIndex2) = (head tokenOrderWithRepeats, (head . tail) tokenOrderWithRepeats)
        tokensEqual <- dc3TokenCompare ioDoubledInputTokens tokenIndex1 tokenIndex2
        let rank' = if tokensEqual then rank else rank + 1
        let revResult' = rank':revResult
        indicesWithoutRepeatsHelper ioDoubledInputTokens (tail tokenOrderWithRepeats) revResult' rank' (i + 1) bound


dc3TokenCompare :: IOArray Index [StrChar] -> Index -> Index -> IO Bool
dc3TokenCompare ioDoubledInputTokens t1 t2  = do
    token1 <- readArray ioDoubledInputTokens t1
    token2 <- readArray ioDoubledInputTokens t2
    return $ token1 == token2

strToDC3Tokens :: [StrChar] -> [[StrChar]]
strToDC3Tokens [] = []
strToDC3Tokens str = token:(strToDC3Tokens rest)
    where
        (token, rest) = splitAt 3 str

translateToNewAlphabet :: [Index] -> [Index] -> IO [StrChar]
translateToNewAlphabet tokenOrderWithRepeats indicesWithRepeatsRemoved = do
    tokenOrder <- newArray_ (0, (length tokenOrderWithRepeats) - 1)
    translateToNewAlphabetHelper tokenOrder tokenOrderWithRepeats indicesWithRepeatsRemoved
    indexArr <- getElems tokenOrder
    return $ map (\i -> (PseudoEOF i)) indexArr


translateToNewAlphabetHelper :: IOArray Index Index -> [Index] -> [Index] -> IO ()
translateToNewAlphabetHelper _ [] [] = return ()
translateToNewAlphabetHelper _ [] _ = error "BLERGH tokenOrderWithRepeats and indicesWithRepeatsRemoved should be the same length!!!"
translateToNewAlphabetHelper _ _ [] = error "BLERGH tokenOrderWithRepeats and indicesWithRepeatsRemoved should be the same length!!!"
translateToNewAlphabetHelper tokenOrder tokenOrderWithRepeats indicesWithRepeatsRemoved = do
    writeArray tokenOrder (head tokenOrderWithRepeats) (head indicesWithRepeatsRemoved)
    translateToNewAlphabetHelper tokenOrder (tail tokenOrderWithRepeats) (tail indicesWithRepeatsRemoved)

mapDoubledArrayTokensToMaster :: [Index] -> [Index]
mapDoubledArrayTokensToMaster tokenIndices = map (doubledArrayTokenIndicesToMaster $ length tokenIndices) tokenIndices


doubledArrayTokenIndicesToMaster :: Int -> Int -> Index
doubledArrayTokenIndicesToMaster tokenIndicesLength i = t1T2IndexToMasterIndex unshuffledIndex
    where 
        shift = if i < ((tokenIndicesLength + 1) `quot` 2) then 0 else tokenIndicesLength - 1  + (tokenIndicesLength `mod` 2)
        unshuffledIndex = i * 2 - shift

t1T2IndexToMasterIndex :: Index -> Index
t1T2IndexToMasterIndex t1T2Index = (t1T2Index `quot` 2)*3 + (t1T2Index `mod` 2) + 1


unsort :: [Index] -> Int -> IO [Index]
unsort sortedIndices mappingSpace = do
    result <- newArray (0, mappingSpace - 1) 0
    unsortHelper result sortedIndices 0
    getElems result

unsortHelper :: IOArray Index Index -> [Index] -> Int -> IO ()
unsortHelper _ [] _ = return ()
unsortHelper result (x:xs) i = do
    writeArray result x i 
    unsortHelper result xs (i + 1)

getT0Ordering :: [StrChar] -> Int -> Int -> Int -> [Index] ->  IO [Index]
getT0Ordering str strlen alphabetSize numEOFs unsortedRanks = do
    let tokens = getT0Tokens str strlen unsortedRanks -- tokens is int []
    mapT0ToMaster <$> (radixSort tokens 2 (max alphabetSize strlen) numEOFs)


getT0Tokens :: [StrChar] -> Int -> [Index] -> [StrChar]
getT0Tokens str strlen unsortedRanks = getT0TokensHelper revStr revStrLen revUnsortedRanks []
    where
        nToLastT0 = (strlen - 1) `mod` 3 
        revStr = drop nToLastT0 (reverse str)
        revStrLen = strlen - nToLastT0
        revUnsortedRanks = if nToLastT0 == 0 
                            then (-1):(reverse unsortedRanks) 
                            else drop (nToLastT0 - 1) (reverse unsortedRanks)

getT0TokensHelper :: [StrChar] -> Int -> [Index] -> [StrChar] -> [StrChar]
getT0TokensHelper _ 0 _ tokens = tokens
getT0TokensHelper revStr revStrLen revUnsortedRanks tokens = 
    getT0TokensHelper revStr' (revStrLen - numToDrop) revUnsortedRanks' tokens'
    where
        (ch, rankCh) = (head revStr, PseudoEOF (head revUnsortedRanks))
        tokens' = ch:rankCh:tokens
        numToDrop = min 3 revStrLen
        dropUpTo3 = drop numToDrop
        (revStr', revUnsortedRanks') = (dropUpTo3 revStr, dropUpTo3 revUnsortedRanks)

radixSort :: [StrChar] -> Int -> Int -> Int -> IO [Index]
radixSort tokens tokenSize alphabetSize numEOFs = do
    let tokensLen = length tokens
    ioTokens <- newListArray (0, tokensLen - 1)tokens
    radixSortHelper ioTokens tokenSize tokensLen alphabetSize numEOFs (tokenSize - 1) Nothing

 
radixSortHelper :: IOArray Index StrChar -> Int -> Int -> Int -> Int -> Int -> Maybe [Index] -> IO [Index]
radixSortHelper tokens tokenSize tokensLen alphabetSize numEOFs rnd maybePartiallySortedIndices
    | rnd < 0 = do
        let sortedIndices = case maybePartiallySortedIndices of
                            Nothing -> error "Partially sorted indices shouldn't be NULL"
                            Just x -> x
        return $ sortedIndices
    | otherwise = do
        buckets <- newArray (-1, numEOFs + alphabetSize - 1) []
        fillBuckets tokens tokenSize tokensLen buckets numEOFs rnd maybePartiallySortedIndices
        sortedIndices <- emptyBuckets buckets alphabetSize numEOFs
        radixSortHelper tokens tokenSize tokensLen alphabetSize numEOFs (rnd - 1) (Just sortedIndices)

fillBuckets :: IOArray Index StrChar -> Int -> Int -> IOArray Index [Index] -> Int -> Int -> Maybe [Index] -> IO ()
fillBuckets tokens tokenSize tokensLen buckets numEOFs rnd maybePartiallySortedIndices = 
    fillBucketsHelper tokens tokenSize buckets numEOFs rnd maybePartiallySortedIndices 0 (tokensLen `quot` tokenSize)

fillBucketsHelper :: IOArray Index StrChar -> Int -> IOArray Index [Index] -> Int -> Int -> Maybe [Index] -> Index -> Index -> IO ()
fillBucketsHelper tokens tokenSize buckets numEOFs rnd maybePartiallySortedIndices i bound 
    | i == bound = return ()
    | otherwise = do
        let (tokenIndex, maybePartiallySortedIndices') = case maybePartiallySortedIndices of 
                            Nothing -> (i, Nothing) 
                            Just partiallySortedIndices -> (head partiallySortedIndices, Just $ tail partiallySortedIndices)
        let chIndex = tokenIndex * tokenSize + rnd
        ch <- readArray tokens chIndex
        let strChrIndexValue = strCharToIndex ch numEOFs
        bucketContents <- readArray buckets strChrIndexValue
        writeArray buckets strChrIndexValue (tokenIndex:bucketContents)
        fillBucketsHelper tokens tokenSize buckets numEOFs rnd maybePartiallySortedIndices' (i + 1) bound

emptyBuckets :: IOArray Index [Index] -> Int -> Int -> IO [Index]
emptyBuckets buckets alphabetSize numEOFs = emptyBucketsHelper buckets (-1) (alphabetSize + numEOFs)

emptyBucketsHelper :: (IOArray Index [Index]) -> Index -> Index -> IO [Index]
emptyBucketsHelper buckets i bound 
    | i == bound = return [] -- The lowest index possible is -1 (there is a PseudoEOF that can have that value)
    | otherwise = do
        bucketContents <- readArray buckets i
        rest <- emptyBucketsHelper buckets (i + 1) bound
        return $ (reverse bucketContents) ++ rest

strCharToIndex :: StrChar -> Int -> Index
strCharToIndex strChar numEOFs = 
    case strChar of
        PseudoEOF i -> i
        ActualChar ch -> numEOFs + (fromEnum ch)

charToIndex :: Char -> Char -> Index
charToIndex ch eof = 
    if ch == eof then 0 else (fromEnum ch) + 1

mapT0ToMaster :: [Index] -> [Index]
mapT0ToMaster sortedIndices = map t0IndexToMasterIndex sortedIndices


t0IndexToMasterIndex :: Index -> Index
t0IndexToMasterIndex t0Index = t0Index * 3


mergeT0WithRest :: [StrChar] -> Int -> [Index] -> [Index] -> [Index] -> IO [Index]
mergeT0WithRest str strLen t0Order t1t2Order unsortedRanks = do
    ioInput <- newListArray (0, strLen - 1) str
    ioUnsortedRanks <- newListArray (0, strLen - 1) unsortedRanks
    suffixArrayResult <- newArray_ (0, strLen - 1)
    mergeT0WithRestHelper ioInput ioUnsortedRanks t0Order t1t2Order suffixArrayResult 0
    getElems suffixArrayResult



mergeT0WithRestHelper :: IOArray Index StrChar -> IOArray Index Index -> [Index] -> [Index] -> IOArray Index Index -> Index -> IO ()
mergeT0WithRestHelper _ _ [] t1t2Order suffixArrayResult i = mergeT0WithRestFinish t1t2Order suffixArrayResult i
mergeT0WithRestHelper _ _ t0Order [] suffixArrayResult i = mergeT0WithRestFinish t0Order suffixArrayResult i
mergeT0WithRestHelper ioInput ioUnsortedRanks t0Order t1t2Order suffixArrayResult i = do
    t0TokenLessThanT1T2Token <- suffixCompare ioInput ioUnsortedRanks t0Order t1t2Order
    if t0TokenLessThanT1T2Token 
        then do
            writeArray suffixArrayResult i (head t0Order)
            mergeT0WithRestHelper ioInput ioUnsortedRanks (tail t0Order) t1t2Order suffixArrayResult (i + 1)
        else do
            writeArray suffixArrayResult i (head t1t2Order)
            mergeT0WithRestHelper ioInput ioUnsortedRanks t0Order (tail t1t2Order) suffixArrayResult (i + 1)

mergeT0WithRestFinish :: [Index] -> IOArray Index Index -> Index -> IO ()
mergeT0WithRestFinish [] _ _ = return ()
mergeT0WithRestFinish rest suffixArrayResult i = do
    writeArray suffixArrayResult i (head rest)
    mergeT0WithRestFinish (tail rest) suffixArrayResult (i + 1) 

suffixCompare :: IOArray Index StrChar -> IOArray Index Index -> [Index] -> [Index] -> IO Bool
suffixCompare input unsortedRanks t0Order t1t2Order = do
    let t0CharIndex = head t0Order
    let t1t2CharIndex = head t1t2Order
    t0Char <- readArray input t0CharIndex
    t1t2Char <- readArray input t1t2CharIndex
    suffixCompareHelper input unsortedRanks t0Char t0CharIndex t1t2Char t1t2CharIndex


suffixCompareHelper :: IOArray Index StrChar -> IOArray Index Index -> StrChar -> Index -> StrChar -> Index -> IO Bool
suffixCompareHelper input unsortedRanks t0Char t0CharIndex t1t2Char t1t2CharIndex 
    | t0Char /= t1t2Char = return $ t0Char < t1t2Char
    | otherwise = do
        let (t0CharIndex', t1t2CharIndex') = (t0CharIndex + 1, t1t2CharIndex + 1)
        t0Char' <- readArray input t0CharIndex'
        t1t2Char' <- readArray input t1t2CharIndex'
        suffixCompareHelper input unsortedRanks t0Char' t0CharIndex' t1t2Char' t1t2CharIndex'



appendEOF :: String -> Index -> [StrChar]
appendEOF [] i = (PseudoEOF i):[]
appendEOF (x:xs) i = (ActualChar x):(appendEOF xs i)

getToFromGeneralMaps :: [[StrChar]] -> SuffixRankings -> IO (Index -> IO (StrNum, Index), (StrNum, Index) -> IO Index)
getToFromGeneralMaps strs sa = do
    let totalLength = length $ concat strs 
    let strsWithIndices = zip [0..] strs --[(0, "foo$"), (1, "bar$"), (2, "a$")]
    let strsOfIndices = map (\(i, str) -> replicate (length str) i) strsWithIndices -- [[0, 0, 0, 0], [1, 1, 1, 1], [2, 2]]
    let individualStrAddresses = concat $ map (\x -> zip x [0..]) strsOfIndices -- [(0, 0), (0, 1), (0, 2), (0, 3), (1, 0), (1, 1), (1, 2), ...]
    ioIndividualStrAddresses <- newListArray (0, totalLength - 1) individualStrAddresses
    let toIndividualStrAddr = toIndividualStrAddr' ioIndividualStrAddresses
    cumSumOfLengths <- newListArray (0, length strs) $ 0 : (scanl1 (+) $ map length strsOfIndices)
    strIToSAI <- invertSA sa
    let strIndexToSuffixArrayIndex = strIndexToSuffixArrayIndex' cumSumOfLengths strIToSAI
    return (toIndividualStrAddr, strIndexToSuffixArrayIndex)

toIndividualStrAddr' :: IOArray Index (StrNum, Index) -> Index -> IO (StrNum, Index)
toIndividualStrAddr' ioIndividualStrAddresses i = readArray ioIndividualStrAddresses i

strIndexToSuffixArrayIndex' :: IOArray Index Int -> IOArray Index Index -> (StrNum, Index) -> IO Index
strIndexToSuffixArrayIndex' cumSumOfLengths strIToSAI (strNum, i) = do 
    strStart <- readArray cumSumOfLengths strNum
    readArray strIToSAI (strStart + i)

invertSA :: SuffixRankings -> IO(IOArray Index Index)
invertSA sa = do
    strIToSAI <- newArray_ (0, (length sa) -1)
    invertSAHelper sa 0 strIToSAI

invertSAHelper :: SuffixRankings -> Index -> IOArray Index Index -> IO(IOArray Index Index)
invertSAHelper [] _ strIToSAI = return strIToSAI
invertSAHelper sa i strIToSAI = do
    writeArray strIToSAI (head sa) i
    invertSAHelper (tail sa) (i + 1) strIToSAI

-- The following two functions will need to implement the algorithm in http://www.cs.iastate.edu/~cs548/references/linear_lcp.pdf
lCPInfo :: SuffixRankings -> [StrChar] -> IO LCPInfo
lCPInfo indices inputString = do
    let len = length indices
    pos <- newListArray (0, len - 1) indices
    rank <- newArray_ (0, len - 1)
    loadRankArray pos rank 0 len
    inputStrArray <- newListArray (0, (length inputString) - 1) inputString
    height <- newArray (0, len - 1) 0
    loopOverRankArray pos rank height inputStrArray 0 (pred len) 0
    return height

-- Helper for lCPInfo
loopOverRankArray :: IOArray Index Index -> IOArray Index Index -> IOArray Index Int -> IOArray Index StrChar -> Index -> Index -> Int -> IO ()
loopOverRankArray pos rank height string i end h 
        | i == end = return ()
        | otherwise =
          do rankI <- readArray rank i
             if rankI > 1 then do
                k <- readArray pos (rankI - 1)
                h' <- incrementH string i k 0
                writeArray height rankI h'
                let h'' = if h' > 0 then h' - 1 else h'
                loopOverRankArray pos rank height string (succ i) end h''
             else loopOverRankArray pos rank height string (succ i) end h
             
-- Helper for lCPInfo
incrementH :: IOArray Index StrChar -> Index -> Index -> Int -> IO Int
incrementH string i k h = do
    stringIPlusH <- readArray string (i + h)
    stringKPlusH <- readArray string (k + h)
    if stringIPlusH == stringKPlusH then do
        incrementH string i k (succ h)
    else do
        return h

-- Helper for lCPInfo
loadRankArray :: IOArray Index Index -> IOArray Index Index -> Index -> Index -> IO ()
loadRankArray pos rank i end 
        | i == end = return ()
        | otherwise = do 
            val <- readArray pos i
            writeArray rank val i
            loadRankArray pos rank (i + 1) end

-- Takes a string and a user provided eof character that doesn't appear anywhere in the string
toBurrowsWheeler :: String -> Char -> IO String
toBurrowsWheeler str eof = do
    let str' = appendEOF str 0
    let strLen = length str'
    sa <- strToSuffixArray str' strLen initialAlphabetSize 1
    ioStr <- newListArray (0, strLen - 1) str'
    suffixArrayToBurrowsWheeler sa ioStr eof

suffixArrayToBurrowsWheeler :: [Index] -> IOArray Index StrChar -> Char -> IO String
suffixArrayToBurrowsWheeler [] _ _ = return ""
suffixArrayToBurrowsWheeler sa ioStr eof = do
    let i = (head sa) - 1
    ch <- if i < 0 
            then return eof
            else do
                strChar <- readArray ioStr i
                case strChar of 
                    ActualChar c -> return c
                    _ -> error "SuffixArray should have only one 0"
    bwtRest <- suffixArrayToBurrowsWheeler (tail sa) ioStr eof
    return $ ch:bwtRest

-- Takes a string and a user provided eof character that doesn't appear anywhere in the string
fromBurrowsWheeler :: String -> Char -> IO String
fromBurrowsWheeler bwt eof = do
    let bwtLen = length bwt
    (p, c) <- firstPass bwt bwtLen (initialAlphabetSize + 1) eof
    secondPass c (initialAlphabetSize + 1)
    s <- thirdPass p c bwt bwtLen eof
    return $ init s

firstPass :: String -> Int -> Int -> Char -> IO (IOArray Index Index, IOArray Index Index)
firstPass bwt bwtLen alphabetSize eof = do
    p <- newArray_ (0, bwtLen - 1) 
    c <- newArray (0, alphabetSize - 1) 0
    firstPassHelper bwt p c 0 bwtLen eof

firstPassHelper :: String -> IOArray Index Index -> IOArray Index Index -> Index -> Index -> Char -> IO (IOArray Index Index, IOArray Index Index)
firstPassHelper bwt p c i n eof
    | i == n = return (p, c)
    | otherwise = do
        let l_i_ch = head bwt
        let l_i = charToIndex l_i_ch eof
        c_at_l_i <- readArray c l_i
        writeArray p i c_at_l_i
        writeArray c l_i (c_at_l_i + 1)
        firstPassHelper (tail bwt) p c (i + 1) n eof

secondPass :: IOArray Index Index -> Int -> IO ()
secondPass c alphabetSize = secondPassHelper c alphabetSize 0 0

secondPassHelper :: IOArray Index Index -> Int -> Int -> Index -> IO ()
secondPassHelper c alphabetSize curSum curCh 
    | curCh == alphabetSize = return ()
    | otherwise = do
        c_ch <- readArray c curCh
        let curSum' = curSum + c_ch
        writeArray c curCh (curSum' - c_ch)
        secondPassHelper c alphabetSize curSum' (curCh + 1)

thirdPass :: IOArray Index Index -> IOArray Index Index -> String -> Int -> Char -> IO String
thirdPass p c bwt bwtLen eof = do
    let eofIndex = case (elemIndex eof bwt) of
                        Nothing -> error "ERROR: BWT must contain EOF"
                        Just i -> i
    ioBWT <- newListArray (0, (length bwt) - 1) bwt
    thirdPassHelper p c ioBWT eof eofIndex (bwtLen - 1) "" 

thirdPassHelper :: IOArray Index Index -> IOArray Index Index -> IOArray Index Char -> Char -> Index -> Index -> String -> IO String
thirdPassHelper p c bwt eof i j decodedStr 
    | j < 0 = return decodedStr
    | otherwise = do
        l_i_ch <- readArray bwt i
        let decodedStr' = l_i_ch:decodedStr
        let l_i = charToIndex l_i_ch eof
        c_at_l_i <- readArray c l_i
        p_i <- readArray p i
        let i' = p_i + c_at_l_i
        thirdPassHelper p c bwt eof i' (j - 1) decodedStr'

-- Longest common extension. Array of indices must match number of strings in GeneralizedSuffixArray
lce :: GeneralizedSuffixArray -> [Index] -> IO Int
lce sa indices = do
    addresses <- mapM (strIndexToSAIndex sa) (zip [0..] indices)
    genLcpList <- getElems $ genLcp sa
    indexOfLengthOfLCE <- if length indices /= numInputStrs sa 
                            then error "must provide same number of indices as there are in the GeneralizedSuffixArray" 
                            else (genLcpRMQ sa) ((minimum addresses) + 1) (maximum addresses)
    readArray (genLcp sa) (indexOfLengthOfLCE)
