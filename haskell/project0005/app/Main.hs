module Main (main) where

main :: IO ()
main = print $ foldr lcm 1 [1..20]::[Integer]
