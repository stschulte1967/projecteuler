module Main (main) where

main :: IO ()
--main = print $ largestFactor 600851475143
main = print $ largestFactor 13195
--    where
--        largestFactor n = head [x | x <- [n, n-1..1], n `mod` x == 0, isPrime x]
--        isPrime m = null [y | y <- [2..(floor . sqrt . fromIntegral) m], m `mod` y == 0]

largestFactor :: Integer -> Integer
largestFactor n | n < 1 = error "Input must be a positive integer"
                | otherwise = largestFactorUnsafe n

largestFactorUnsafe :: Integer -> Integer
largestFactorUnsafe 1 = 1
largestFactorUnsafe 2 = 2
largestFactorUnsafe n = head $ lf n n 3 [1]

lf :: Integer -> Integer -> Integer -> [Integer] -> [Integer]
lf n_orig n d acc
    | product acc == n_orig = acc
    | even n = lf n_orig (n `div` 2) (if even (n `div` 2) then 2 else 3) (2:acc)
    | mod n d == 0 = lf n_orig (n `div` d) d (d:acc)
    | otherwise     = lf n_orig n (d + 2) acc

 