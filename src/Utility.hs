module Utility (Index, (!-!), (!-!-!), (!-!-!=), new2DArray_, new2DArray, loop, loopInner, loopOuter) where

import Data.Array.IO

type Index = Int

(!-!) :: IOArray Index Int -> Index -> IO Int
arr !-! i = do readArray arr i

(!-!-!) :: IOArray (Index, Index) Int -> (Index, Index) -> IO Int
twoDArr !-!-! (i, j) = do readArray twoDArr (i, j)

(!-!-!=) :: IOArray (Index, Index) Int -> (Index, Index, Int) -> IO ()
twoDArr !-!-!= (i, j, val) = do writeArray twoDArr (i, j) val


new2DArray_ :: Int -> Int -> IO (IOArray (Index, Index) Int)
new2DArray_ x y = do newArray_ ((0, 0), (x - 1, y - 1))


new2DArray :: Int -> Int -> Int -> IO (IOArray (Index, Index) Int)
new2DArray x y e = do newArray ((0, 0), (x - 1, y - 1)) e

loop :: IOArray Index Int -> (Index, Index) -> (IOArray Index Int -> Index -> IO ()) -> IO ()
loop arr (i, boundI) f
    | i == boundI = do return ()
    | otherwise = do
        f arr i
        loop arr (i + 1, boundI) f

loopInner :: IOArray (Index, Index) Int -> Index -> (Index, Index) -> (IOArray (Index, Index) Int -> (Index, Index) -> IO ()) -> IO ()
loopInner arr i (j, boundJ) f
    | j == boundJ = do return ()
    | otherwise = do
        f arr (i, j)
        loopInner arr i (j + 1, boundJ) f

loopOuter :: IOArray (Index, Index) Int -> (Index, Index) -> (Index -> (Index, Index)) -> (IOArray (Index, Index) Int -> (Index, Index) -> IO ()) -> IO ()
loopOuter arr (i, boundI) calcJBounds f
    | i == boundI = do return ()
    | otherwise = do
        loopInner arr i (calcJBounds i) f
        loopOuter arr (i + 1, boundI) calcJBounds f


