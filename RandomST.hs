{-# OPTIONS -Wall -Werror #-}

module RandomST
	( State()
	, randomST
	, randomRST
	, randomRSTList
	, RandomGen
	, Random
	) where

import Control.Monad.State
import System.Random

randomST :: (RandomGen g, Random a) => State g a
randomST = state random

randomRST :: (RandomGen g, Random a) => (a, a) -> State g a
randomRST = state . randomR

randomRSTList :: (RandomGen g, Random a) => [(a, a)] -> State g [a]
randomRSTList = sequence . foldr (\x acc -> randomRST x : acc) []
