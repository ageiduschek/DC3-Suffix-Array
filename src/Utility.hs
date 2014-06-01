module Utility (Index, (!-!), (!-!-!), (!-!-!=), new2DArray_, new2DArray, loop, loopInner, loopOuter) where

import Data.Array.IO

type Index = Int

-- | Gets value at index i from a 1D mutable IOArray, arr
(!-!) :: IOArray Index Int -> Index -> IO Int
arr !-! i = do readArray arr i

-- | Gets value at index (i, j) from a 2D mutable IOArray, arr
(!-!-!) :: IOArray (Index, Index) Int -> (Index, Index) -> IO Int
arr !-!-! (i, j) = do readArray arr (i, j)

-- | Sets value at index (i, j) of 2D mutable IOArray, arr
(!-!-!=) :: IOArray (Index, Index) Int -> (Index, Index, Int) -> IO ()
arr !-!-!= (i, j, val) = do writeArray arr (i, j) val

-- | Creates a new uninitialized 2D mutable array with size x in the x dimension and size y in the y dimension
new2DArray_ :: Int -> Int -> IO (IOArray (Index, Index) Int)
new2DArray_ x y = do newArray_ ((0, 0), (x - 1, y - 1))

-- | Creates a new 2D mutable array with size x in the x dimension and size y in the y dimension. All values initialized to e.
new2DArray :: Int -> Int -> Int -> IO (IOArray (Index, Index) Int)
new2DArray x y e = do newArray ((0, 0), (x - 1, y - 1)) e

-- | Loops through each index from i to boundI - 1 of arr, performing the function f for each value of i.
loop :: IOArray Index Int -> (Index, Index) -> (IOArray Index Int -> Index -> IO ()) -> IO ()
loop arr (i, boundI) f
    | i == boundI = do return ()
    | otherwise = do
        f arr i
        loop arr (i + 1, boundI) f

-- | Loops over a 2D array. The param calcJ bounds calculates the values of j to loop over for a given i. This performs the function f for every (i, j) pair it loops over.
loopOuter :: IOArray (Index, Index) Int -> (Index, Index) -> (Index -> (Index, Index)) -> (IOArray (Index, Index) Int -> (Index, Index) -> IO ()) -> IO ()
loopOuter arr (i, boundI) calcJBounds f
    | i == boundI = do return ()
    | otherwise = do
        loopInner arr i (calcJBounds i) f
        loopOuter arr (i + 1, boundI) calcJBounds f


loopInner :: IOArray (Index, Index) Int -> Index -> (Index, Index) -> (IOArray (Index, Index) Int -> (Index, Index) -> IO ()) -> IO ()
loopInner arr i (j, boundJ) f
    | j == boundJ = do return ()
    | otherwise = do
        f arr (i, j)
        loopInner arr i (j + 1, boundJ) f


