module Main where

import List
import ListContinued
import ListAgain

main :: IO ()
main = do
    -- xs <- randomPermutation "abcdef"
    putStrLn . show $ combinations 4 "abcde"