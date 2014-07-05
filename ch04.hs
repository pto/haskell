-- Fizzbuzz

import Data.List (intercalate)

fizzBuzzLine :: Int -> String
fizzBuzzLine x | x `mod` 15 == 0 = "FizzBuzz"
               | x `mod` 3 == 0  = "Fizz"
               | x `mod` 5 == 0  = "Buzz"
               | otherwise       = show x

fizzBuzzList :: Int -> [String]
fizzBuzzList n = map fizzBuzzLine [1..n]

fizzBuzz :: IO ()
fizzBuzz = putStrLn (intercalate "\n" (fizzBuzzList 100))

-- Triangle

nextValue :: [[Int]] -> Int
nextValue xss = last (last xss) + 1

line :: Int -> Int -> [Int]
line x size = [x..x + size - 1]

triangle :: Int -> [[Int]]
triangle 1 = [[1]]
triangle x = triangle (x - 1) ++ [line (nextValue (triangle (x - 1))) x]


