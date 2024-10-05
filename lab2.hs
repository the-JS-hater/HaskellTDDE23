--check_pnr([7, 4, 0, 2, 1, 7, 4, 8, 2, 0])
--True
--check_pnr([7, 4, 0, 2, 1, 7, 4, 8, 2, 1])
--False

check_pnr :: [Integer] -> Bool
check_pnr pnr = (sum_pnr (init pnr) + last pnr) `mod` 10 == 0

sum_pnr :: [Integer] -> Integer
sum_pnr (x : xs : xss) = split_and_sum x + xs + sum_pnr xss
sum_pnr [x] = split_and_sum x 
sum_pnr [] = 0

split_and_sum :: Integer -> Integer
split_and_sum x = (x * 2 `div` 10) + (x * 2 `mod` 10)
