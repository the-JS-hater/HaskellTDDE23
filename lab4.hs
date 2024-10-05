import Data.Char (isLower, isUpper)

split :: String -> IO()
split message = 
  display $ split_message message
  

split_message :: String -> (String, String)
split_message [] = ("", "")  
split_message (x : xs)
  | isLower x    = let (first, second) = split_message xs in (x : first, second)
  | x == '.'     = let (first, second) = split_message xs in (x : first, second)
  | x == '_'     = let (first, second) = split_message xs in (x : first, second)
  | isUpper x    = let (first, second) = split_message xs in (first, x : second)
  | x == '|'     = let (first, second) = split_message xs in (first, x : second)
  | x == ' '     = let (first, second) = split_message xs in (first, x : second)
  | otherwise    = split_message xs  

display :: (String, String) -> IO ()
display (first_message, second_message) = putStrLn $ first_message ++ " " ++ second_message
