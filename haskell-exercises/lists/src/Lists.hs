module Lists (member, union, intersection, difference,
              insert, insertionSort,
              binaryToDecimal, toDecimal, toDec, decimal,
              binaryAdd, firsts) where

import Data.Char (digitToInt, intToDigit)

member :: Int -> [Int] -> Bool
member _ []      = False
member e (x:xs)  = e == x || member e xs

union :: [Int] -> [Int] -> [Int]
union [] ys     = ys
union (x:xs) ys
  | member x ys = union xs ys
  | otherwise   = x : union xs ys

intersection :: [Int] -> [Int] -> [Int]
intersection xs ys = [x | x <- xs, member x ys]

difference :: [Int] -> [Int] -> [Int]
difference xs ys = [x | x <- xs, not (member x ys)]

insert :: Int -> [Int] -> [Int]
insert e [] = [e]
insert e (x:xs)
  | e <= x    = e : x : xs
  | otherwise = x : insert e xs

insertionSort :: [Int] -> [Int]
insertionSort = foldr insert []

binaryToDecimal :: [Int] -> Int
binaryToDecimal xs = sum $ zipWith (\x y -> x * 2^y) (reverse xs) [0..]

toDecimal :: Int -> [Int] -> Int
toDecimal base digits = sum $ zipWith (\x y -> x * base^y) (reverse digits) [0..]

toDec :: Int -> String -> Int
toDec base s = sum $ zipWith (\x y -> x * base^y) (reverse $ map digitToInt s) [0..]

decimal :: Int -> String -> Int
decimal base s = sum [digitToInt x * base^y | (x, y) <- zip (reverse s) [0..]]

firsts :: [a] -> [[a]]
firsts xs = [take n xs | n <- [1..length xs]]

binaryAdd :: String -> String -> String
binaryAdd "" "" = "0"
binaryAdd a b = reverse $ add (reverse a) (reverse b) 0
  where
    add [] [] 0 = []
    add [] [] c = [intToDigit c]
    add (x:xs) [] c = add (x:xs) "0" c
    add [] (y:ys) c = add "0" (y:ys) c
    add (x:xs) (y:ys) c =
      let sum = digitToInt x + digitToInt y + c
          (carry, digit) = sum `divMod` 2
      in intToDigit digit : add xs ys carry

merge:: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
