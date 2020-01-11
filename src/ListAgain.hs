module ListAgain (
    insertAt,
    range,
    randomSelect,
    lotto,
    randomPermutation,
    combinations
) where 

import System.Random
import Control.Monad
import ListContinued

insertAt :: Int -> a -> [a] -> [a]
insertAt n a xs     
    | n == 0 || null xs = a : xs
    | n < 0  = error "insertAt: negative index"
insertAt n a (x:xs) = x : insertAt (n-1) a xs

range :: Int -> Int -> [Int]
range = enumFromTo

randomSelect :: Int -> [a] -> IO [a]
randomSelect n xs
    | n < 0     = error "randomSelect: negative index"
    | otherwise = do
        rs <- replicateM n rdnIndex
        return $ map (xs !!) rs
        where l = length xs
              rdnIndex = getStdRandom $ randomR (0, l - 1)

lotto :: Int -> Int -> IO [Int]
lotto n m = randomSelect n [1..m]

randomPermutation :: [a] -> IO [a]
randomPermutation [] = return []
randomPermutation xs = do
    (y, ys) <- extract xs
    zs <- randomPermutation ys 
    return (y:zs)
    where rdnIndex n = getStdRandom $ randomR (0, n)
          extract xs = fmap (`removeAt` xs) $ rdnIndex $ length xs - 1

combinations :: Int -> [a] -> [[a]]
combinations n xs 
    | n < 0 = error "combinations: negative index"
    | n == 0 = [[]]
    | n > (length xs) = []
combinations n (x:xs) = (map (x:) (combinations (n - 1) xs)) ++ (combinations n xs)