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
    --private float [] elems;
    --private int blockSize;
    
    --private SparseTableRMQ summarySparseTable;
    --private SparseTableRMQ [] blockSparseTables;
    --private Map<Integer, Integer> blockSignatures;

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



{-
    public FischerHeunRMQ(float[] elems) {
        //ensures that the block size is at least 1
        blockSize = getFHBlockSize(elems); 
        this.elems = elems;
        float [] blockMinValues = createBlockSummaries(elems);
        summarySparseTable = new SparseTableRMQ(blockMinValues); 
    }
-}

getFHBlockSize :: Int -> Int
getFHBlockSize len = undefined

    --private int getFHBlockSize(float [] elems) {
    --    return (int)((Math.log(elems.length)/Math.log(2))/4 + 1);
    --}

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



    --private float [] createBlockSummaries(float [] elems){
    --    float [] blockMinValues  = new float[numTotalBlocks()];
    --    blockSparseTables = new SparseTableRMQ [(int)Math.pow(4, getFHBlockSize(elems))];
    --    blockSignatures = new HashMap<Integer, Integer>();

    --    //Loop runs n/b times
    --    for(int i = 0; i < blockMinValues.length; i++){


    --        int blockStartIndex = i * blockSize;

    --        //O(b)
    --        int blockRMQKey = generateRMQKey(elems, blockStartIndex);
    --        blockSignatures.put(i, blockRMQKey);

    --        //This happens at most 4^b times
    --        if(blockSparseTables[blockRMQKey] == null){               
    --            //O(b)
    --            float [] block = Arrays.copyOfRange(elems, blockStartIndex, Math.min(blockStartIndex + blockSize, elems.length));

    --            //O(blog(b))
    --            blockSparseTables[blockRMQKey] = new SparseTableRMQ(block);
    --        }

    --        int blockMinIndex = getBlockMinIndex(i, 0, elems.length - 1);
    --        blockMinValues[i] = elems[blockMinIndex];

    --    }
    --    return blockMinValues;
    --}


createOrAssignBlockSummaries :: [Int] -> Int -> IOArray Index Int -> IOArray Index (Maybe SparseTable) -> IOArray BlockNum Index -> Int -> Index -> Index -> IO ()
createOrAssignBlockSummaries a aLength blockMinVals blockSTs blockSigs bSize i bound
        | i == bound = do return ()
        | otherwise = do
            let blockEndIndex = min aLength $ bSize
            let blockRMQKey = generateRMQKey a [] 0 0 0 blockEndIndex
            writeArray blockSigs i blockRMQKey
            currST <- readArray blockSTs blockRMQKey
            case currST of 
                Just _ -> return ()  
                Nothing -> do
                    let block = take blockEndIndex a 
                    st <- sparseTableRMQ block
                    writeArray blockSTs blockRMQKey (Just st)
            blockMinIndex <- getBlockMinIndex blockSTs blockSigs i 0 (aLength - 1)
            writeArray blockMinVals i $ a !! blockMinIndex
            let a' = drop blockEndIndex a 
            createOrAssignBlockSummaries a' (aLength - blockEndIndex) blockMinVals blockSTs blockSigs bSize (i + 1) bound


numTotalBlocks :: Int -> Int -> Int
numTotalBlocks elemsLength bSize = quot (elemsLength + bSize - 1) bSize 

--private int numTotalBlocks(){
--    return (elems.length + blockSize - 1)/blockSize;
--}


rmq :: FH -> Index -> Index -> IO Index
rmq fh i j = undefined
{-
    public int rmq(int i, int j) {
        int startBlock = blockNum(i);
        int endBlock = blockNum(j);

        int startMin = getBlockMinIndex(startBlock, i, j); //O(blockSize) = O(log(n))
        int endMin = getBlockMinIndex(endBlock, i, j); //O(blockSize) = O(log(n))

        int min = elems[startMin] < elems[endMin] ? startMin : endMin;
        
        // //If there are blocks in the middle
        if(endBlock - startBlock > 1){

            //Find the block that contains the minimum value
            int middleMinBlockNum = summarySparseTable.rmq(startBlock + 1, endBlock - 1); //O(1)

            //Get the block signature
            int blockSignature = blockSignatures.get(middleMinBlockNum);

            //Calculate the start and end indices of that block
            int blockStartIndex = middleMinBlockNum * blockSize;
            int startIndex = Math.max(blockStartIndex, i);
            int endIndex = Math.min(blockStartIndex + blockSize  - 1, j);

            //look up that block using it's RMQ table
            int middleMinIndex = blockStartIndex + blockSparseTables[blockSignature].rmq(startIndex - blockStartIndex, endIndex - blockStartIndex); //O(1)
            min = elems[min] < elems[middleMinIndex] ? min : middleMinIndex;
        }
        return min;
    }

    private int blockNum(int index) {
        return index/blockSize;
    }
-}

getBlockMinIndex :: IOArray Index (Maybe SparseTable) -> IOArray BlockNum Index -> Int -> Index -> Index -> IO Int
getBlockMinIndex blockSTs blockSigs blockNum minIndex maxIndex = undefined



{-
    private int getBlockMinIndex(int blockNum, int minIndex, int maxIndex) {
        int blockStartIndex = blockNum * blockSize;
        int startIndex = Math.max(blockStartIndex, minIndex);
        int endIndex = Math.min(blockStartIndex + blockSize  - 1, maxIndex);

        int blockSignature = blockSignatures.get(blockNum);
        SparseTableRMQ blockSparseTable = blockSparseTables[blockSignature];
        return blockStartIndex + blockSparseTable.rmq(startIndex - blockStartIndex, endIndex - blockStartIndex);
    }
-}



generateRMQKey :: [Int] -> [Int] -> Int -> Index -> Index -> Index -> Int
generateRMQKey a ctStack bitVector bitIndex i bound 
    | i == bound = bitVector
    | otherwise = generateRMQKey a' ctStack' bitVector' bitIndex' (i + 1) bound
        where
            a_i = head a
            (a', bitIndex') = popBitStack ctStack a_i bitIndex
            ctStack' = a_i:ctStack
            bitVector' = setBit bitVector bitIndex' 



popBitStack :: [Int] -> Int -> Index -> ([Int], Int)
popBitStack [] _ bitIndex = ([], bitIndex)
popBitStack ctStack@(x:xs) elems_i bitIndex
    | head ctStack <= elems_i = (ctStack, bitIndex)
    | otherwise = popBitStack xs elems_i (bitIndex + 1) 


{-
    private int generateRMQKey(float [] elems, int blockStartIndex) {
        BitSet bitSet = new BitSet(Integer.SIZE);
        Stack<Float> cartesianTreeStack = new Stack<Float>();

        int bitIndex = 0;
        for(int i = blockStartIndex; i < Math.min(blockStartIndex + blockSize, elems.length); i++) {
            while(!cartesianTreeStack.empty() && cartesianTreeStack.peek() > elems[i]) {
                float popped = cartesianTreeStack.pop();
                bitIndex++;
            }
            // if(elems.length == 1000) System.out.println("Push " + elems[i]);
            cartesianTreeStack.push(elems[i]);
            bitSet.set(bitIndex);
            bitIndex++;
        }

        return bitSetToInt(bitSet);
    }


-}

