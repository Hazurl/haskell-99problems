module List (
    myLast,
    myButLast,
    elementAt,
    myLength,
    myReverse,
    isPalindrome,
    myFlatten,
    compress,
    pack,
    encode,

    NestedList(..)
) where 

import Control.Arrow
import Control.Monad

myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs
-- myLast = last

myButLast :: [a] -> a
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

elementAt :: Int -> [a] -> a
elementAt n | n < 1 = error "Invalid index"
elementAt 1 = head
elementAt i = tail >>> elementAt (i - 1)
-- elementAt i xs = xs !! (i - 1)

myLength :: [a] -> Int
myLength = foldl (((+ 1) .). const) 0
-- myLength = length

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = and $ zipWith (==) xs (reverse xs)
-- isPalindrome = liftM2 (==) id reverse

data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List l) = concatMap myFlatten l

compress :: Eq a => [a] -> [a]
compress = reverse . go []
    where
        go :: Eq a => [a] -> [a] -> [a]
        go xs [] = xs
        go [] (y:ys) = go [y] ys
        go (x:xs) (y:ys)
            | x == y    = go (x:xs) ys
            | otherwise = go (y:x:xs) ys 

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = go [x] xs
    where
        go :: Eq a => [a] -> [a] -> [[a]]
        go xs []        = [xs] 
        go xs@(x:_) (y:ys)
            | x == y    = go (y:xs) ys
            | otherwise = xs : go [y] ys 
-- pack = group

encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = go [x] xs
    where
        go :: Eq a => [a] -> [a] -> [(Int, a)]
        go xs@(x:_) []  = [(length xs, x)]
        go xs@(x:_) (y:ys)
            | x == y    = go (y:xs) ys
            | otherwise = (length xs, x) : go [y] ys 
-- encode xs = [(length x, head x) | x <- group xs]