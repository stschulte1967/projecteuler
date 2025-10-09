module Main (main) where

main :: IO ()
main = print $ sum base ^ 2 - sum (map (^2) base)  
  where base = [1..100]