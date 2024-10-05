choose :: Integer -> Integer -> Integer 
choose n k  
  | n == k = 1
  | k > n - k = factorial n k `div` factorial (n - k) 1
  | n - k > k = factorial n (n - k) `div` factorial k 1

factorial :: Integer -> Integer -> Integer
factorial n stop 
  | n == stop = 1 
  | otherwise = n * (factorial (n - 1) stop)
