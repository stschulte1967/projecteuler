module Main (main) where

main :: IO ()
main = print $ sum $ filter (\x -> mod x 3 == 0 || mod x 5 == 0) ([1..999] :: [Int])
