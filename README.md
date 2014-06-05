Stanford CS240H Final Project - Ben Isaacs and Anna Geiduschek
==============================================================

API
---

SparseTable.hs:

    -- Returns function that can be used to find RMQ over two indices on the array.
    --         Calls to sparseTableRMQ run in O(nlog(n)) time with O(1) time queries
    --         to the ouput function.

<code>
    type SparseTable = Index -> Index -> IO Index
    sparseTableRMQ :: [Int] -> IO SparseTable
</code>

FisherHeun.hs:

    -- Returns function that can be used to find RMQ over two indices on the array.
    --         Calls to fischerHeunRMQ run in O(n) time with O(1) time queries
    --         to the ouput function.
<code>
    type FischerHeun = Index -> Index -> IO Index
    fischerHeunRMQ :: [Int] -> IO FischerHeun
</code>

SuffixArray.hs:

    -- Functions to create SuffixArray types which can then be passed around
<code>
    createSuffixArray :: String -> IO SuffixArray
    createGeneralizedSuffixArray :: [String] -> IO GeneralizedSuffixArray
</code>

    -- Returns a lexicographically ordered list of indices which each
    --        represent a suffix in the original input string.
<code>
    getSuffixRankings :: SuffixArray -> [Int]
 </code>   
    -- Returns a lexicographically ordered list of (string number, index) 
    --        pairs which each represent a suffix in one of the original
    --  input strings.
<code>
    getGeneralizedSuffixRankings :: GeneralizedSuffixArray -> [(Int, Int)]
</code>

    -- Retrieve the original input string(s) from a suffix array.
<code>
    getInputStr :: SuffixArray -> String
    getInputStrs :: GeneralizedSuffixArray -> [String]
</code>

    -- For demonstrative purposes.
<code>
    printSuffixes :: SuffixArray -> IO ()
    printGeneralizedSuffixes :: GeneralizedSuffixArray -> IO ()
</code>

    -- For Generalized suffix array with input strings S1 to Sn, accepts 
    --        an array of indices [i1, …, in] and returns the length of 
    --        the shared prefix between S1[i1], ... , Sn[in] . The array 
    --        of indices must be the same length as the array of input 
    --        strings. Example usage:
    --            ghci> gsa <- createGeneralizedSuffixArray [“anna”, “banana”]
    --            ghci> lce gsa [0, 1]
    --            2
    --            ghci> lce gsa [2, 3]
    --            0
<code>    
    lce :: GeneralizedSuffixArray -> [Index] -> IO Int
</code>

    -- Takes a string and an EOF character that doesn’t appear anywhere in the
    --       and returns the Burrows Wheeler transform on that string. To recover 
    --       the original string, pass the result of this function to 
    --       fromBurrowsWheeler with the same EOF character. Example usage:
    --            ghci> bwt <- toBurrowsWheeler "Today you are you! That is truer than  
    --            true!" '$'
    --            ghci> bwt
    --            "!!utrnsyeeu $h hdoruutT aTyyeattia   oorra  "
<code>
    toBurrowsWheeler :: String -> Char -> IO String
</code>

    -- Takes a string that has been transformed by toBurrowsWheeler and recovers the
    --       original string. Must use the same EOF character that was given to
    --       toBurrowsWheeler. Example usage:
    --            ghci> str <- toBurrowsWheeler "!!utrnsyeeu $h hdoruutT aTyyeattia   
                  oorra  " '$'
    --            ghci> str
    --            "Today you are you! That is truer than true!"
<code>
    fromBurrowsWheeler :: String -> Char -> IO String
</code>
