-- Print pi to a specified number of digits using Machin's formula

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then 
        error "Usage: pi <number of digits>"
    else
        let digits = read (head args)
            in putStrLn (π digits)

-- Pi with the given number of digits
π :: Integer -> String
π digits = 
    let pi = show (4 * (4 * arccot 5 digits - arccot 239 digits) `div` 10^10)
        in [head pi] ++ "." ++ tail pi
    
-- Inverse cotangent of x multiplied by 10^(digits + 10)
arccot :: Integer -> Integer -> Integer
arccot x digits = 
    let unity = 10 ^ (digits + 10)
        in arccotIter x 1 (unity `div` x)
    where 
        arccotIter :: Integer -> Integer -> Integer -> Integer
        arccotIter x coefficient term
            | term == 0 = 0
            | otherwise = term `div` coefficient +
                arccotIter x (coefficient + 2) (term `div` (negate (x*x)))
