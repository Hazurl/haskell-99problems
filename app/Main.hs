module Main where

import List
import ListContinued

main :: IO ()
main = putStrLn . show $ removeAt 12 "0123456789"