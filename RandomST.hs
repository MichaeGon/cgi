{-# OPTIONS -Wall -Werror #-}

module RandomST
	( State()
	, randomST
	, randomRST
	, random
	, randomR
	, starray
	, mkStdGen
	, getStdGen
	, runState
	, RandomGen
	, Random
	) where

import Control.Monad.State
import System.Random

randomST :: (RandomGen g, Random a) => State g a
randomST = state random

randomRST :: (RandomGen g, Random a) => (a, a) -> State g a
randomRST = state . randomR

starray :: (RandomGen g, Random a) => [(a, a)] -> State g [a]
starray = sequence . foldr (\x acc -> randomRST x : acc) []

