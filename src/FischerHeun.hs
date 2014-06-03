module FischerHeun (FischerHeun, fischerHeunRMQ) where

import Utility
import SparseTable
import Data.Array.IO
import Data.Bits

type FischerHeun = Index -> Index -> IO Index
type BlockNum = Index

data FH = FH { elems :: IOArray Index Int, 
            blockSize :: Int, 
            summarySparseTable :: SparseTable,
            blockSparseTables :: IOArray Index (Maybe SparseTable),
            blockSignatures :: IOArray BlockNum Index
        } 

fischerHeunRMQ :: [Int] -> IO FischerHeun
fischerHeunRMQ a = do
    let elemsLength = length a
    elms <- newListArray (0, elemsLength - 1) a
    let bSize = getFHBlockSize elemsLength
    (blockMinVals, blockSTs, blockSigs) <- createBlockSummaries a elemsLength bSize
    summaryST <- sparseTableRMQ blockMinVals
    return $ rmq $ FH {elems = elms
                        , blockSize = bSize
                        , summarySparseTable = summaryST 
                        , blockSparseTables = blockSTs
                        , blockSignatures = blockSigs
                    }

getFHBlockSize :: Int -> Int
getFHBlockSize len = 1 + (quot logBase2OfLen 4)
    where 
        float2 = (fromIntegral (2::Int)) :: Double
        floatLen = fromIntegral len :: Double
        xxx = (logBase float2 floatLen)
        logBase2OfLen = truncate xxx

createBlockSummaries :: [Int] -> Int -> Int -> IO ([Int], IOArray Index (Maybe SparseTable), IOArray BlockNum Index)
createBlockSummaries a elemsLength bSize = do
    let numBlocks = numTotalBlocks elemsLength bSize
    blockMinVals <- newArray_ (0, numBlocks - 1)
    blockSTs <- newArray (0, (numDifferentSTs bSize)) Nothing
    blockSigs <- newArray_ (0, numBlocks) 
    createOrAssignBlockSummaries a elemsLength blockMinVals blockSTs blockSigs bSize 0 numBlocks
    blockMinValsArr <- getElems blockMinVals
    return (blockMinValsArr, blockSTs, blockSigs)

numDifferentSTs :: Int -> Int
numDifferentSTs bSize = 4 ^ bSize


createOrAssignBlockSummaries :: [Int] -> Int -> IOArray Index Int -> IOArray Index (Maybe SparseTable) -> IOArray BlockNum Index -> Int -> Index -> Index -> IO ()
createOrAssignBlockSummaries a aLength blockMinVals blockSTs blockSigs bSize i bound
        | i == bound = do return ()
        | otherwise = do
            let blockEndIndex = min aLength bSize
            blockRMQKey <- generateRMQKey a [] 0 0 0 blockEndIndex
            writeArray blockSigs i blockRMQKey
            currST <- readArray blockSTs blockRMQKey
            case currST of 
                Just _ -> return ()  
                Nothing -> do
                    let block = take blockEndIndex a
                    st <- sparseTableRMQ block
                    writeArray blockSTs blockRMQKey (Just st)
            maybeST <- readArray blockSTs blockRMQKey
            case maybeST of
                Nothing -> error "foobarsdfsdfsdf"
                Just myST -> do
                    blockMinIndex <- myST 0 (bSize - 1)
                    writeArray blockMinVals i $ a !! blockMinIndex
                    let a' = drop blockEndIndex a 
                    createOrAssignBlockSummaries a' (aLength - blockEndIndex) blockMinVals blockSTs blockSigs bSize (i + 1) bound


generateRMQKey :: [Int] -> [Int] -> Int -> Index -> Index -> Index -> IO Int
generateRMQKey a ctStack bitVector bitIndex i bound
    | i == bound = do return bitVector
    | otherwise = do 
        generateRMQKey (tail a) ctStack'' bitVector' (bitIndex' + 1) (i + 1) bound
        where
            a_i = head a
            (ctStack', bitIndex') = popBitStack ctStack a_i bitIndex
            ctStack'' = a_i:ctStack'
            bitVector' = setBit bitVector bitIndex' 

popBitStack :: [Int] -> Int -> Index -> ([Int], Int)
popBitStack [] _ bitIndex = ([], bitIndex)
popBitStack ctStack@(x:xs) elems_i bitIndex
    | x <= elems_i = (ctStack, bitIndex)
    | otherwise = popBitStack xs elems_i (bitIndex + 1) 

numTotalBlocks :: Int -> Int -> Int
numTotalBlocks elemsLength bSize = quot (elemsLength + bSize - 1) bSize 


rmq :: FH -> Index -> Index -> IO Index
rmq fh i j = do
    let a = elems fh
    let bSize = blockSize fh
    let (startBlock, endBlock) = (quot i bSize, quot j bSize)
    let (blockSigs, blockSTs) = (blockSignatures fh, blockSparseTables fh)
    let fGetBlockMinIndex = getBlockMinIndex blockSTs blockSigs i j bSize
    startMinIndex <- fGetBlockMinIndex startBlock
    endMinIndex <- fGetBlockMinIndex endBlock 
    startMinVal <- readArray a startMinIndex
    endMinVal <- readArray a endMinIndex

    let minIndex = if startMinVal < endMinVal then startMinIndex else endMinIndex
    if endBlock - startBlock <= 1
        then return minIndex
        else do
            middleMinBlockNum <- (summarySparseTable fh) (startBlock + 1) (endBlock - 1)
            blockSig <- readArray blockSigs middleMinBlockNum

            let blockStartIndex = middleMinBlockNum * bSize
            let startIndex = max blockStartIndex i
            let endIndex = min (blockStartIndex + bSize  - 1) j

            maybeMiddleBlockSTRMQ <- readArray blockSTs blockSig
            case maybeMiddleBlockSTRMQ of 
                Nothing -> error "YOU DONE FUCKED UP"
                Just middleBlockSTRMQ -> do
                    relativeMinIndex <- middleBlockSTRMQ (startIndex - blockStartIndex) (endIndex - blockStartIndex)
                    let middleMinIndex = blockStartIndex + relativeMinIndex
                    minValA <- readArray a minIndex
                    minValB <- readArray a middleMinIndex
                    if minValA < minValB then return minIndex else return middleMinIndex

getBlockMinIndex :: IOArray Index (Maybe SparseTable) -> IOArray BlockNum Index -> Int -> Index -> Index -> Int -> IO Int
getBlockMinIndex blockSTs blockSigs minIndex maxIndex bSize blockNum = do
    let blockStartIndex = blockNum * bSize
    let startIndex = max blockStartIndex minIndex
    let endIndex = min (blockStartIndex + bSize  - 1) maxIndex
    blockSig <- readArray blockSigs blockNum
    maybeBlockST <- readArray blockSTs blockSig
    case maybeBlockST of 
        Nothing -> error "BEN. THERE'S A BUG."
        Just blockST -> do
            indexOfMin <- blockST (startIndex - blockStartIndex) (endIndex - blockStartIndex)
            return $ blockStartIndex + indexOfMin