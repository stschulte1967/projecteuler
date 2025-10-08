module Main (main) where

main :: IO ()
--main = print $ sumEvenFibonacci 100
main = print $ sum $ filter even $ fibhelper 4000000 1 0 [] where
    fibhelper :: Int -> Int -> Int -> [Int] -> [Int]
    fibhelper upperlimit n_m_1 n_m_2 acc | n_m_1 + n_m_2 > upperlimit = acc
                                         | otherwise                  = fibhelper upperlimit (n_m_1 + n_m_2) n_m_1 ((n_m_1 + n_m_2):acc)
