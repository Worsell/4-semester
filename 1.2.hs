module Main where

import Text.Printf

factorial :: Int -> Int
fibonacci n
  | n == 0 = 0
  | n == 1 = 1
  otherwise = fibonacci' 0 1 n

fibonacci' :: Int -> Int -> Int -> Int
fibonacci' curentFibonacci nextfibonacci n
  | n == 0 = currentFibonacci
  | otherwise = fibonacci' nextFibonai (nextFibonacci + curentFibonacci)(n-1)
