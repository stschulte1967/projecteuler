module Main (main) where

main :: IO ()
main = print $ maximum $ [x*y | x <- [100..999], y <- [100..999], let p = x * y, isPalindrome p]
    where
        isPalindrome m = let s = show m in s == reverse s

