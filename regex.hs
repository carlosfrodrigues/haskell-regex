module Regex
(
    generateRegexReq
,   executeMultipleRegex
) where
import Data.List
import Data.Maybe

data RegexReq = RegexReq { characters :: Sentence,
                           minValue :: Quantity,
                           maxValue :: Quantity
} deriving (Show, Eq)
data Quantity = Finite Int | Infinite deriving (Show, Eq)
data Sentence = Digit
              | Word
              | Whitespace
              | Everything
              | Value String
              deriving (Show, Eq)

(!!!) array (m, n) = drop m (take n array)

safeGet :: String -> String -> Int -> Bool
safeGet str values index | length str > index = (str !! index) `elem` values
                            | otherwise =  False

generateRegexReq :: String -> [RegexReq]
generateRegexReq x | (safeGet x "*+?{" 1) =
                       case (x !! 1) of
                            '*' -> RegexReq {characters = Value [(x !! 0)], minValue = Finite 0, maxValue = Infinite }:generateRegexReq(x !!! (2, length x))
                            '+' -> RegexReq {characters = Value [(x !! 0)], minValue = Finite 1, maxValue = Infinite }:generateRegexReq(x !!! (2, length x))
                            '?' -> RegexReq {characters = Value [(x !! 0)], minValue = Finite 0, maxValue = Finite 1 }:generateRegexReq(x !!! (2, length x))
                            '{' -> RegexReq {characters = Value "teste", minValue = Finite 1, maxValue = Finite 1 }:generateRegexReq(x !!! (2, length x))
                   | (safeGet x "(" 0 ) =
                       [RegexReq {characters = Value "teste2", minValue = Finite 1, maxValue = Finite 1 }]
                   | (safeGet x "[" 0) =
                        [RegexReq {characters = Value "teste2", minValue = Finite 1, maxValue = Finite 1 }]
                   | (length x == 0) = []
                   | ((length (takeWhile (`notElem` "*+?{(") x)) == length x) =
                        [RegexReq {characters = Value x, minValue = Finite 1, maxValue = Finite 1}]
                   | otherwise = let value = takeWhile (`notElem` "*+?{(") x
                                in if((safeGet x "([" (length value))) then
                                        RegexReq {characters = Value value, minValue = Finite 1, maxValue = Finite 1 }:generateRegexReq(x !!! (length value, length x))
                                    else
                                        RegexReq {characters = Value (init value), minValue = Finite 1, maxValue = Finite 1 }:generateRegexReq(x !!! ((length value) - 1, length x))


executeMultipleRegex :: [RegexReq] -> String -> Maybe [String]        
executeMultipleRegex r str = do
                                  let list = map (\x -> executeRegex x str) r
                                  if(Nothing `elem` list) then
                                      Nothing
                                  else do
                                    let nubList = map nub  (map (fromMaybe [""]) list)
                                    let combinations = makeCombinations nubList
                                    Just (filter (\x -> isInfixOf x str) combinations)
                                    
                                    


makeCombinations :: [[String]] -> [String]
makeCombinations (x:xs) | length (x:xs) > 1 = [(a ++ b) | a <- x, b <- makeCombinations(xs)]
                        | otherwise = (x:xs) !! 0

isExpInit :: String -> String -> Bool
isExpInit exp str | 0 `elem` (findString exp str) = True
                  | otherwise = False

executeRegex :: RegexReq -> String -> Maybe [String]
executeRegex (RegexReq characters minValue maxValue) str | checkData characters "Value" = 
                                                            findValue str (getValue characters) minValue maxValue
                                                         | checkData characters "Digit" = Nothing
                                                         | checkData characters "Word" = Nothing
                                                         | checkData characters "Whitespace" = Nothing
                                                         | checkData characters "Everything" = Nothing
                                                         | otherwise = Nothing
getValue :: Sentence -> String
getValue (Value str) = str

getQuantity :: Quantity -> Int
getQuantity (Finite number) = number

checkData :: (Show t) => t -> String -> Bool
checkData typeToCheck str = isPrefixOf  (str ++ " ") (show typeToCheck)

findValue :: String -> String -> Quantity -> Quantity -> Maybe [String]
findValue str exp (Finite minValue) maxValue | (minValue == 0) && (show maxValue == "Infinite") = Just $ getSubstrings exp str
                                             | (minValue == 0) && (checkData maxValue "Finite") = Just $ filter (\x -> (length x) `div` (length exp) <= (getQuantity maxValue)) $ getSubstrings exp str
                                             | (minValue == 1) && (show maxValue == "Infinite") = if ((getSubstrings exp str) == []) then Nothing else Just (getSubstrings exp str)
                                             | (minValue == 1) && (show maxValue == "Finite 1") = 
                                                do 
                                                let listresult = filter (\x -> (length x) == (length exp)) $ getSubstrings exp str;
                                                if (listresult == []) then
                                                    Nothing 
                                                else
                                                    Just listresult
                                             where substrings = findString exp str

{-
findDigit ::
findWord ::
findWhitespace ::
findEverything ::
-}

findString :: String -> String -> [Int]
findString search str = findIndices (isPrefixOf search) (tails str)


getSubstrings :: String -> String -> [String]
getSubstrings search str = let list = findRepetitions (length search) (findString search str)
                            in [str !!! (fst x, (fst x) + (snd x))| x <- list ]


findRepetitions :: Int -> [Int] -> [(Int, Int)]
findRepetitions size (x:xs) = let tuple = (x , size*(1 + (length (takeWhile (diffElements size ) (tails (x:xs))) )))
                               in tuple:findRepetitions size (xs !!! ((((snd tuple) `div` size) - 1), length xs))
findRepetitions size [] = []

diffElements :: Int -> [Int] -> Bool
diffElements size (a:b:_) = (b - a == size)
diffElements size [a] = False
diffElements size [] = False

main::IO()
main = undefined