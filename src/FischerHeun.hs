module FischerHeun (FischerHeun, fischerHeunRMQ) where

import Utility
import SparseTable

type FischerHeun = Index -> Index -> IO Index

data FH = FH {} 
    --private float [] elems;
    --private int blockSize;
    
    --private SparseTableRMQ summarySparseTable;
    --private SparseTableRMQ [] blockSparseTables;
    --private Map<Integer, Integer> blockSignatures;

fischerHeunRMQ :: [Int] -> IO FischerHeun
fischerHeunRMQ a = undefined

{-
    public FischerHeunRMQ(float[] elems) {
        //ensures that the block size is at least 1
        blockSize = getFHBlockSize(elems); 
        this.elems = elems;
        float [] blockMinValues = createBlockSummaries(elems);
        summarySparseTable = new SparseTableRMQ(blockMinValues); 
    }
-}

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
-}
{-
    private int blockNum(int index) {
        return index/blockSize;
    }

    private int getBlockMinIndex(int blockNum, int minIndex, int maxIndex) {
        int blockStartIndex = blockNum * blockSize;
        int startIndex = Math.max(blockStartIndex, minIndex);
        int endIndex = Math.min(blockStartIndex + blockSize  - 1, maxIndex);

        int blockSignature = blockSignatures.get(blockNum);
        SparseTableRMQ blockSparseTable = blockSparseTables[blockSignature];
        return blockStartIndex + blockSparseTable.rmq(startIndex - blockStartIndex, endIndex - blockStartIndex);
    }

    private int numTotalBlocks(){
        return (elems.length + blockSize - 1)/blockSize;
    }

    private float [] createBlockSummaries(float [] elems){
        float [] blockMinValues  = new float[numTotalBlocks()];
        blockSparseTables = new SparseTableRMQ [(int)Math.pow(4, getFHBlockSize(elems))];
        blockSignatures = new HashMap<Integer, Integer>();

        //Loop runs n/b times
        for(int i = 0; i < blockMinValues.length; i++){


            int blockStartIndex = i * blockSize;

            //O(b)
            int blockRMQKey = generateRMQKey(elems, blockStartIndex);
            blockSignatures.put(i, blockRMQKey);

            //This happens at most 4^b times
            if(blockSparseTables[blockRMQKey] == null){               
                //O(b)
                float [] block = Arrays.copyOfRange(elems, blockStartIndex, Math.min(blockStartIndex + blockSize, elems.length));

                //O(blog(b))
                blockSparseTables[blockRMQKey] = new SparseTableRMQ(block);
            }

            int blockMinIndex = getBlockMinIndex(i, 0, elems.length - 1);
            blockMinValues[i] = elems[blockMinIndex];

        }
        return blockMinValues;
    }

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

    private int bitSetToInt(BitSet bitSet)
    {
        int bitInteger = 0;
        for(int i = 0 ; i < Integer.SIZE; i++)
            if(bitSet.get(i)) {
                bitInteger |= (1 << i);   
            }

        return bitInteger;
    }

    private int getFHBlockSize(float [] elems) {
        return (int)((Math.log(elems.length)/Math.log(2))/4 + 1);
    }
-}

