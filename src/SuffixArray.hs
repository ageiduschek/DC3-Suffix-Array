module SuffixArray (SuffixArray, GeneralizedSuffixArray, createSuffixArray, createGeneralizedSuffixArray, toBurrowsWheeler, fromBurrowsWheeler, lce) where

import Utility
import FischerHeun
--import Data.HashTable as HT
import Control.Applicative
import Data.Array.IO

type Length = Int
type StrNum = Index

type LCPInfo = [Length]

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
                                                        , strIndexToOverallIndex :: (StrNum, Index) -> Index 
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
    lCPRMQ <- fischerHeunRMQ lCP
    return $ SuffixArrayConstructor {inputStr = str', orderedSuffixes = sa, lcp = lCP, lcpRMQ = lCPRMQ}


-- Creates a generalized suffix array out of a list of strings
createGeneralizedSuffixArray :: [String] -> IO GeneralizedSuffixArray
createGeneralizedSuffixArray strs = do
    let inputStrings = zipWith (appendEOF) strs [0..]
    let (toIndividualStrAddr, toOverallAddr) = getToFromGeneralMaps inputStrings
    let str' = (concat inputStrings)
    sa <- strToSuffixArray str' (length str') initialAlphabetSize (length inputStrings)
    let arr = map toIndividualStrAddr sa

    let lCP = undefined --genLCPInfo arr inputStrings
    lCPRMQ <- return undefined --fischerHeunRMQ lCP
    return $ GeneralizedSuffixArrayConstructor {  inputStrs = inputStrings 
                                                , genOrderedSuffix = arr 
                                                , numInputStrs = length inputStrings
                                                , strIndexToOverallIndex = toOverallAddr  
                                                , genLcp = lCP 
                                                , genLcpRMQ = lCPRMQ  
                                            }

-- This performs the DC3 Algorithm
strToSuffixArray :: [StrChar] -> Int -> Int -> Int -> IO [Index]
strToSuffixArray [_] _ _ _ = do return [0]
strToSuffixArray str strlen alphabetSize numEOFs = do
    --putStrLn $ "In strToSuffixArray. "
    --putStrLn $ "str: " ++ (show str)
    --putStrLn $ "strlen: " ++ (show strlen)
    t1t2Order <- getT1AndT2Ordering str strlen alphabetSize numEOFs
    unsortedRanks <- unsort t1t2Order strlen
    t0Order <- getT0Ordering str strlen alphabetSize numEOFs unsortedRanks
    mergeT0WithRest str strlen t0Order t1t2Order unsortedRanks

--strToSuffixArray str strlen alphabetSize numEOFs = return $ [8, 7, 4, 0, 5, 2, 1, 6, 3] -- Suffix array for "nonsense"

getT1AndT2Ordering :: [StrChar] -> Int -> Int -> Int -> IO [Index]
getT1AndT2Ordering str strlen alphabetSize numEOFs = do
    --putStrLn $ "Entering call to getT1AndT2Ordering"
    let (doubledInput, doubledInputLen) = shiftAndDouble str strlen
    --putStrLn $ "doubledInput = " ++ show doubledInput
    --putStrLn $ "doubledInputLen = " ++ show doubledInputLen
    tokenOrderWithRepeats <- radixSort doubledInput 3 alphabetSize numEOFs
    --putStrLn $ "tokenOrderWithRepeats = " ++ show tokenOrderWithRepeats
    (indicesWithRepeatsRemoved, newAlphabetSize) <- indicesWithoutRepeats doubledInput doubledInputLen tokenOrderWithRepeats
    --putStrLn $ "indicesWithRepeatsRemoved = " ++ show indicesWithRepeatsRemoved
    --putStrLn $ "newAlphabetSize = " ++ show newAlphabetSize
    tokensInNewAlphabet <- translateToNewAlphabet tokenOrderWithRepeats indicesWithRepeatsRemoved
    --putStrLn $ "tokensInNewAlphabet = " ++ show tokensInNewAlphabet
    twoThirdsSuffixArray <- strToSuffixArray tokensInNewAlphabet (length tokensInNewAlphabet) newAlphabetSize newAlphabetSize
    --putStrLn $ "Returned from recursive call"
    return $ mapDoubledArrayTokensToMaster twoThirdsSuffixArray

--getT1AndT2Ordering str strlen alphabetSize numEOFs = return $ [8, 7, 4, 5, 2, 1]

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

    --private int[] shiftAndDouble(int [] input) {
    --    int totalPads = input.length % 3 == 0 ? 3 : input.length % 3;
    --    int [] doubledInput = new int[input.length * 2 + totalPads - 3];

    --    int i = 1, j = 2;

    --    for(; i < input.length; i++) doubledInput[i - 1] = input[i]; //Add string with first char removed
    --    for(;(i - 1)%3 != 0; i++) doubledInput[i - 1] = 0; //Pad with EOF

    --    for(; j < input.length; j++) doubledInput[i + j - 3] = input[j]; //Add string with first two chars removed
    --    for(;(j - 2)%3 != 0; j++) doubledInput[i + j - 3] = 0; //Pad with EOF

    --    return doubledInput;
    --}


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

    --private int [] indicesWithoutRepeats(int [] doubledInput, int [] tokenOrderWithRepeats) {
    --    int [] result = new int [tokenOrderWithRepeats.length]; //init to 0 or put a 0 in the first spot
    --    int rank = 0;
    --    for(int i = 1; i < tokenOrderWithRepeats.length; i++) {
    --        if(!dc3TokenCompare(doubledInput, tokenOrderWithRepeats[i-1], tokenOrderWithRepeats[i])) {
    --            rank++;
    --        }
    --        result[i] = rank;
    --    }

    --    return result;
    --}

    --int newAlphabetSize = indicesWithRepeatsRemoved[indicesWithRepeatsRemoved.length -1] + 1;


    --private boolean dc3TokenCompare(int [] doubledInput, int t1, int t2) {
    --    for(int i=0; i < 3; i++) {
    --        int ch1 = doubledInput[t1 * 3 + i];
    --        int ch2 = doubledInput[t2 * 3 + i];
    --        if(ch1 != ch2) return false;
    --    }
    --    return true;
    --}







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

    --int [] tokenOrder = new int[tokenOrderWithRepeats.length];
    --for(int i = 0; i < tokenOrderWithRepeats.length; i++) {
    --    tokenOrder[tokenOrderWithRepeats[i]] = indicesWithRepeatsRemoved[i];
    --}

mapDoubledArrayTokensToMaster :: [Index] -> [Index]
mapDoubledArrayTokensToMaster tokenIndices = map (doubledArrayTokenIndicesToMaster $ length tokenIndices) tokenIndices


doubledArrayTokenIndicesToMaster :: Int -> Int -> Index
doubledArrayTokenIndicesToMaster tokenIndicesLength i = t1T2IndexToMasterIndex unshuffledIndex
    where 
        shift = if i < ((tokenIndicesLength + 1) `quot` 2) then 0 else tokenIndicesLength - 1  + (tokenIndicesLength `mod` 2)
        unshuffledIndex = i * 2 - shift

t1T2IndexToMasterIndex :: Index -> Index
t1T2IndexToMasterIndex t1T2Index = (t1T2Index `quot` 2)*3 + (t1T2Index `mod` 2) + 1
--private int [] mapDoubledArrayTokensToMaster(int [] tokenIndices) {
--    int [] masterIndices = new int[tokenIndices.length];
--    for(int i=0; i < tokenIndices.length; i++) {
--        int shift = tokenIndices[i] < (tokenIndices.length + 1)/2 ? 0 : tokenIndices.length - 1  + (tokenIndices.length % 2);
--        int unshuffledIndex = tokenIndices[i] * 2 - shift;
--        masterIndices[i] = t1T2IndexToMasterIndex(unshuffledIndex);
--    }
--    return masterIndices;
--}

    --private int t1T2IndexToMasterIndex(int t1T2Index) {
    --    return (t1T2Index/2)*3 + (t1T2Index%2) + 1;
    --}




unsort :: [Index] -> Int -> IO [Index]
unsort sortedIndices mappingSpace = do
    result <- newArray (0, mappingSpace - 1) 0
    unsortHelper result sortedIndices 0
    getElems result


unsortHelper :: IOArray Index Index -> [Index] -> Int -> IO ()
unsortHelper _ [] _ = return ()
unsortHelper result (x:xs) i = do
    writeArray result x i -- result[x] = i
    unsortHelper result xs (i + 1)

{-
    private int[] unsort(int [] sortedIndices, int mappingSpace) {
        int [] result = new int[mappingSpace];
        for(int i=0; i < sortedIndices.length; i++) {
            result[sortedIndices[i]] = i;
        }
        return result;
    }
-}


getT0Ordering :: [StrChar] -> Int -> Int -> Int -> [Index] ->  IO [Index]
getT0Ordering str strlen alphabetSize numEOFs unsortedRanks = do
    let tokens = getT0Tokens str strlen unsortedRanks -- tokens is int []
    mapT0ToMaster <$> (radixSort tokens 2 (max alphabetSize strlen) numEOFs)

--getT0Ordering str strlen alphabetSize numEOFs unsortedRanks = return $ [0, 6, 3]

{-
    private int[] getT0Ordering(int[] input, int [] t1t2Order, int [] unsortedRanks, int alphabetSize){        
        int [] tokens = getT0Tokens(input, unsortedRanks);
    
        int[] sortedIndices = radixSort(tokens, 2, Math.max(alphabetSize, input.length));
        int [] master = mapT0ToMaster(sortedIndices);
        return mapT0ToMaster(sortedIndices);
    }
-}

--testGetT0Tokens :: IO()
--testGetT0Tokens = do
--    let str' = appendEOF "aaaa" 0
--    putStrLn $ show $ getT0Tokens str' (length str') [0, 2, 1, 0, 0]

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

--numT0Tokens :: Int -> Int
--numT0Tokens inputLen = (quot inputLen 3) + (if inputLen `mod` 3 == 0 then 0 else 1)

radixSort :: [StrChar] -> Int -> Int -> Int -> IO [Index]
radixSort tokens tokenSize alphabetSize numEOFs = do
    --putStrLn $ "Entering radixSort"
    --putStrLn $ "tokens: " ++ (show tokens)
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
        --putStrLn $ "round " ++ (show rnd)
        buckets <- newArray (-1, numEOFs + alphabetSize - 1) []
        fillBuckets tokens tokenSize tokensLen buckets numEOFs rnd maybePartiallySortedIndices
        sortedIndices <- emptyBuckets buckets alphabetSize numEOFs
        radixSortHelper tokens tokenSize tokensLen alphabetSize numEOFs (rnd - 1) (Just sortedIndices)


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
        let strChrIndexValue = toIndex ch numEOFs
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

toIndex :: StrChar -> Int -> Index
toIndex strChar numEOFs = 
    case strChar of
        PseudoEOF i -> i
        ActualChar ch -> numEOFs + (fromEnum ch)




mapT0ToMaster :: [Index] -> [Index]
mapT0ToMaster sortedIndices = map t0IndexToMasterIndex sortedIndices


t0IndexToMasterIndex :: Index -> Index
t0IndexToMasterIndex t0Index = t0Index * 3


{-
    private int [] mapT0ToMaster(int [] t0Indices) {
        int [] masterIndices = new int[t0Indices.length];
        for(int i=0; i < t0Indices.length; i++) {
            masterIndices[i] = t0IndexToMasterIndex(t0Indices[i]);
        }
        return masterIndices;
    }

    private int t0IndexToMasterIndex(int t0Index) {
        return t0Index*3;
    }
-}

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

    private boolean suffixCompare(int i, int j, int[] input, int [] t0Order, int [] t1t2Order, int [] unsortedRanks) {
        int t0CharIndex = t0Order[i];
        int t1t2CharIndex = t1t2Order[j];

        while(true){
            int t0Char = input[t0CharIndex];
            int t1t2Char = input[t1t2CharIndex];

            if (t0Char != t1t2Char) return t0Char < t1t2Char;


            if((t0CharIndex + 1) % 3 != 0  && (t1t2CharIndex + 1) % 3 != 0) {
                return unsortedRanks[t0CharIndex + 1] < unsortedRanks[t1t2CharIndex + 1];
            }

            t0CharIndex++;
            t1t2CharIndex++;
        }
    }
-}

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

getToFromGeneralMaps :: [[StrChar]] -> (Index -> (StrNum, Index), (StrNum, Index) -> Index)
getToFromGeneralMaps strs = (toIndividualStrAddr, toOverallAddr)
    where 
        strsWithIndices = zip [0..] strs --[(0, "foo$"), (1, "bar$"), (2, "a$")]
        strsOfIndices = map (\(i, str) -> replicate (length str) i) strsWithIndices -- [[0, 0, 0, 0], [1, 1, 1, 1], [2, 2]]
        individualStrAddresses = concat $ map (\x -> zip x [0..]) strsOfIndices -- [(0, 0), (0, 1), (0, 2), (0, 3), (1, 0), (1, 1), (1, 2), ...]
        toIndividualStrAddr i =  individualStrAddresses !! i
        toOverallAddr (strNum, i) = i + (map length strsOfIndices) !! strNum


-- The following two functions will need to implement the algorithm in http://www.cs.iastate.edu/~cs548/references/linear_lcp.pdf
lCPInfo :: SuffixRankings -> [StrChar] -> IO LCPInfo
lCPInfo indices inputString = do
    let len = length indices
    pos <- newListArray (0, len - 1) indices
    rank <- newArray_ (0, len - 1)
    loadRankArray pos rank 0 len
    rankList <- getElems rank --TODO: DELETE
    putStrLn $ "rankList" ++ (show rankList)
    inputStrArray <- newListArray (0, (length inputString) - 1) inputString
    height <- newArray_ (0, len - 1)
    loopOverRankArray pos rank height inputStrArray 0 len 0
    heightList <- getElems height
    putStrLn $ "heightList" ++ (show heightList)
    getElems height

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

genLCPInfo :: GeneralizedSuffixRankings -> [[StrChar]] -> LCPInfo
--genLCPInfo indices inputStrs = undefined
genLCPInfo _ _ = undefined

-- Takes a string and a user provided eof character that doesn't appear anywhere in the string
toBurrowsWheeler :: String -> Char -> IO String
toBurrowsWheeler str eof = do
    let str' = appendEOF str 0
    let strLen = length str'
    sa <- strToSuffixArray str' strLen initialAlphabetSize 1
    putStrLn $ show sa
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
fromBurrowsWheeler _ _ = undefined
--fromBurrowsWheeler str eof = undefined

-- Longest common extension. Array of indices must match number of strings in GeneralizedSuffixArray
lce :: GeneralizedSuffixArray -> [Index] -> IO Int
lce sa indices = do
    if length indices /= numInputStrs sa 
                    then error "must provide same number of indices as there are in the GeneralizedSuffixArray" 
                    else (genLcpRMQ sa) (minimum addresses) (maximum addresses)
    where 
        addresses = map (strIndexToOverallIndex sa) (zip [0..] indices)
