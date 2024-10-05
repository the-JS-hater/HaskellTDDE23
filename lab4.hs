import Data.Map.Strict as HashMap
import Data.Maybe (fromMaybe)
import Data.Char (isLower, isUpper)

-- LAB 4A ======================
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


-- LAB 4B ======================

data LogicTree = Elem String | List [LogicTree] deriving (Show)

extractElem :: LogicTree -> String
extractElem (Elem s) = s

isElem :: LogicTree -> Bool
isElem (Elem _) = True
isElem _        = False

isList :: LogicTree -> Bool
isList (List _) = True
isList _        = False

interpret :: LogicTree -> HashMap.Map String String -> String
interpret expression interpretation
  | isElem expression = evaluateString (extractElem expression) interpretation
  | List [left, Elem "AND", right] <- expression =
      stringAnd (interpret left interpretation) (interpret right interpretation)
  | List [left, Elem "OR", right] <- expression =
      stringOr (interpret left interpretation) (interpret right interpretation)
  | List [Elem "NOT", expr] <- expression =
      stringNot (interpret expr interpretation)


evaluateString :: String -> HashMap.Map String String -> String
evaluateString variable interpretation =
  fromMaybe variable (HashMap.lookup variable interpretation)

stringAnd :: String -> String -> String
stringAnd "true" "true" = "true"
stringAnd _ _           = "false"

stringOr :: String -> String -> String
stringOr "true" _ = "true"
stringOr _ "true" = "true"
stringOr _ _      = "false"

stringNot :: String -> String
stringNot "true" = "false"
stringNot "false" = "true"
