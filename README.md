Stanford CS240H Final Project - Ben Isaacs and Anna Geiduschek
==============================================================

API
---
<code>
SparseTable.hs:
    -- Returns function that can be used to find RMQ over two indices on the array.
    --         Calls to sparseTableRMQ run in O(nlog(n)) time with O(1) time queries
    --         to the ouput function.
    type SparseTable = Index -> Index -> IO Index
    sparseTableRMQ :: [Int] -> IO SparseTable

FisherHeun.hs:
    -- Returns function that can be used to find RMQ over two indices on the array.
    --         Calls to fischerHeunRMQ run in O(n) time with O(1) time queries
    --         to the ouput function.
    type FischerHeun = Index -> Index -> IO Index
    fischerHeunRMQ :: [Int] -> IO FischerHeun

SuffixArray.hs:
    -- Functions to create SuffixArray types which can then be passed around:
    createSuffixArray :: String -> IO SuffixArray
    createGeneralizedSuffixArray :: [String] -> IO GeneralizedSuffixArray

    -- Returns a lexicographically ordered list of indices which each
    --        represent a suffix in the original input string.
    getSuffixRankings :: SuffixArray -> [Int]
    
    -- Returns a lexicographically ordered list of (string number, index) 
    --        pairs which each represent a suffix in one of the original
    --  input strings.
    getGeneralizedSuffixRankings :: GeneralizedSuffixArray -> [(Int, Int)]

    -- Retrieve the original input string(s) from a suffix array.
    getInputStr :: SuffixArray -> String
    getInputStrs :: GeneralizedSuffixArray -> [String]
    
    -- For demonstrative purposes.
    printSuffixes :: SuffixArray -> IO ()
    printGeneralizedSuffixes :: GeneralizedSuffixArray -> IO ()

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
    
    lce :: GeneralizedSuffixArray -> [Index] -> IO Int

    -- Takes a string and an EOF character that doesn’t appear anywhere in the
    --       and returns the Burrows Wheeler transform on that string. To recover 
    --       the original string, pass the result of this function to 
    --       fromBurrowsWheeler with the same EOF character. Example usage:
    --            ghci> bwt <- toBurrowsWheeler "Today you are you! That is truer than  
    --            true!" '$'
    --            ghci> bwt
    --            "!!utrnsyeeu $h hdoruutT aTyyeattia   oorra  "

    toBurrowsWheeler :: String -> Char -> IO String


    -- Takes a string that has been transformed by toBurrowsWheeler and recovers the
    --       original string. Must use the same EOF character that was given to
    --       toBurrowsWheeler. Example usage:
    --            ghci> str <- toBurrowsWheeler "!!utrnsyeeu $h hdoruutT aTyyeattia   
                  oorra  " '$'
    --            ghci> str
    --            "Today you are you! That is truer than true!"
    fromBurrowsWheeler :: String -> Char -> IO String
</code>
