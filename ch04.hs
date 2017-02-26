-- Exercise 1
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

-- Exercise 2
third :: [a] -> a
third xs = head (tail (tail xs))

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> a
third'' (_:_:x:_) = x

-- Exercise 3
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' xs | null xs   = []
             | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' (_:xs) = xs
safetail'' _      = []

-- Exercise 4
or' :: Bool -> Bool -> Bool
or' False False = False
or' True True   = True
or' True False  = True
or' False True  = True

or'' :: Bool -> Bool -> Bool
or'' False False = False
or'' _ _         = True

or''' :: Bool -> Bool -> Bool
or''' True _ = True
or''' False b = b

or'''' :: Bool -> Bool -> Bool
or'''' a b | a == b    = a
           | otherwise = True

-- Exercise 5
and' :: Bool -> Bool -> Bool
and' a b = if a then if b then True
                          else False
                else False

-- Exercise 6
and'' :: Bool -> Bool -> Bool
and'' a b = if a then b
                 else False

-- Exercise 7
mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x*y*z))

-- Exercise 8
luhnDouble :: Int -> Int
luhnDouble d = if double > 9 then double - 9
                             else double
               where double = d + d

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = calc `mod` 10 == 0
               where calc = luhnDouble a + b + luhnDouble c + d
