module Utility (Index, (!-!), (!-!-!)) where

import Data.Array.IO

type Index = Int

(!-!) :: IOArray Index Int -> Index -> IO Int
arr !-! i = do readArray arr i

(!-!-!) :: IOArray Index (IOArray Index Int) -> (Index, Index) -> IO Int
twoDArr !-!-! (i, j) = do 
    arr <- readArray twoDArr i
    arr !-! j
