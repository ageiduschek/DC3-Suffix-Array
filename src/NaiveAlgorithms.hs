module NaiveAlgorithms (rmq) where

import Utility

{-
Trying to implement this, lol

int rmq (list, start, end) {
	if (end < start) return -1;
	int min = list[start];
	int minIndex = start;
	for (i = start + 1; i <= end; i++) {
		if (list[i] < list[min]) {
			min = list[i];
			minIndex = i;
		}
	}
	return min;
}
-}

rmq :: [Int] -> Index -> Index -> Index
rmq xs start end | end < start = -1
				 | otherwise   = minimumOverRange xs start (start + 1) end

minimumOverRange :: [Int] -> Index -> Index -> Index -> Index
minimumOverRange xs minIndex curr end | curr > end           			= minIndex
							  		  | (xs !! curr) < (xs !! minIndex) = minimumOverRange xs curr (curr + 1) end
							 		  | otherwise            		    = minimumOverRange xs minIndex (curr + 1) end