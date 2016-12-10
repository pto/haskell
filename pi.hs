-- Print pi to a specified number of digits using Machin's formula

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let digits = read (head args)
        in putStrLn ("3." ++ tail (show (π digits)))

π :: Integer -> Integer
π digits = 4 * (4 * arccot 5 digits - arccot 239 digits) `div` 10^10
    
arccot :: Integer -> Integer -> Integer
arccot x digits = 
    let unity = 10 ^ (digits + 10)
        in arccot' x 1 (unity `div` x) 0

-- coefficient is 1, 3, 5, etc.
-- divisor is unity divided by x, -x^3, x^5, -x^7, etc.
-- sum is the result

arccot' :: Integer -> Integer -> Integer -> Integer -> Integer
arccot' x coefficient divisor sum
    | divisor /= 0 = sum + divisor `div` coefficient +
        arccot' x (coefficient + 2) (negate divisor `div` (x*x)) sum
    | otherwise    = sum

