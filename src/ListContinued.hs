module ListContinued (
    encoded,
    decoded,
    duplicate,
    replicateAll,
    dropNth,
    dropEvery,
    split,
    slice,
    rotate,
    removeAt,

    Encoded(..)
) where
 
import Data.List

data Encoded a = Single a | Multiple Int a 
    deriving (Show)

encoded :: Eq a => [a] -> [Encoded a]
encoded = (map go) . group
    where 
        go :: [a] -> Encoded a
        go xs = if n == 1 then Single x else Multiple n x
            where n = length xs
                  x = head xs

decoded :: [Encoded a] -> [a]
decoded = concatMap go
    where 
        go :: Encoded a -> [a]
        go (Single x) = [x]
        go (Multiple n x) = replicate n x

duplicate :: [a] -> [a]
duplicate = concatMap (replicate 2)

replicateAll :: Int -> [a] -> [a]
replicateAll = concatMap . replicate

dropNth :: Int -> [a] -> [a]
dropNth n [] = []
dropNth n cs@(x:xs)
    | n == 0    = xs
    | n < 0     = cs
    | otherwise = x : dropNth (n - 1) xs

dropEvery :: Int -> [a] -> [a]
dropEvery n = go n n 
        where 
            go :: Int -> Int -> [a] -> [a]
            go _ _ [] = []
            go m n cs@(x:xs)
                | n < 0     = cs
                | n == 0    = go m m xs
                | otherwise = x : go m (n - 1) xs

split :: Int -> [a] -> ([a], [a])
split n xs = (take n xs, drop n xs)

slice :: Int -> Int -> [a] -> [a]
slice _ _ [] = []
slice a b cs@(x:xs)
    | b < 0     = []
    | a <= 0    = x : slice (a - 1) (b - 1) xs
    | otherwise = slice (a - 1) (b - 1) xs

rotate :: Int -> [a] -> [a]
rotate n xs = (drop m xs) ++ (take m xs)
    where l = length xs
          m = n `mod` l

removeAt :: Int -> [a] -> (a, [a])
removeAt n (x:xs) 
    | n == 0    = (x, xs)
    | otherwise = f x $ removeAt (n - 1) xs
    where
        f x (y, ys) = (y, x:ys)
