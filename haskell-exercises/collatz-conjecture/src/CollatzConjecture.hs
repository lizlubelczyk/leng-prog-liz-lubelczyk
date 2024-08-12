module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz = collatz' 0
  where
    collatz' :: Integer -> Integer -> Maybe Integer
    collatz' steps n
      | n <= 0 = Nothing
      | n == 1 = Just steps
      | even n = collatz' (steps + 1) (n `div` 2)
      | otherwise = collatz' (steps + 1) (3 * n + 1)
