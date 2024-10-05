divByThree :: Integer -> Bool
divByThree n = mod n 3 == 0

--max2 :: Integral a => a -> a -> a
max2 :: Integer -> Integer -> Integer
max2 a b = if a > b then a else b

max3 :: Integer -> Integer -> Integer -> Integer
--max3 a b c = max2 a (max2 b c)
max3 a b c = max2 a $ max2 b c


