module Fraction (Fraction, add, sub, mul, divide, hcf) where

type Fraction = (Int, Int)

simplify :: Fraction -> Fraction
simplify (n, d) = (n `div` h, d `div` h)
  where h = hcf n d

add :: Fraction -> Fraction -> Fraction
add n d = simplify $
  if den1 == den2 then (num1 + num2, den1)
  else (num1 * den2 + num2 * den1, den1 * den2)
  where
    (num1, den1) = n
    (num2, den2) = d

sub :: Fraction -> Fraction -> Fraction
sub n d = simplify $
  if den1 == den2 then (num1 - num2, den1)
  else (num1 * den2 - num2 * den1, den1 * den2)
  where
    (num1, den1) = n
    (num2, den2) = d

mul :: Fraction -> Fraction -> Fraction
mul n d = simplify $ (num1 * num2, den1 * den2)
  where
    (num1, den1) = n
    (num2, den2) = d

divide :: Fraction -> Fraction -> Fraction
divide n d = simplify $ (num1 * den2, den1 * num2)
  where
    (num1, den1) = n
    (num2, den2) = d

hcf :: Int -> Int -> Int
hcf n d = if d == 0 then n else hcf d (n `mod` d)