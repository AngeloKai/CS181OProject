module Utilities where 

import Data.List
import Data.Char
import System.Random
import Data.List.Split
import Lexicon 


-- Check if a string is an integer 
stringIsInt :: String -> Bool
stringIsInt = all isDigit

-- Convert a string to lowercase 
strToLower :: String -> String
strToLower = map toLower

-- Generate random integer between the range 1 to 3
genRandomInt :: (Num a, Random a) => IO a
genRandomInt = randomRIO (1, 3)

fstLetterUpper :: String -> String
fstLetterUpper (x:xs) = [toUpper x] ++ xs

-- In the spirit of attributing others' contribution:
-- https://www.reddit.com/r/learnprogramming/comments/2anyrj/how_do_you_remove_wordsletters_from_strings_in/
removeWords :: String -> [String] -> String
removeWords list1 words = intercalate " " . filter (not . (`elem` words)) . splitOn " " $ list1

-- In the spirit of attributing others' contribution:
-- http://stackoverflow.com/questions/10114228/delete-all-instances-from-list
deleteAllInstances :: Eq a => a -> [a] -> [a]
deleteAllInstances a list = [x | x <- list, x /= a]

-- Separate a string into a list of strings based on end of line character
iostrToArr :: IO String -> IO [String]
iostrToArr ios = do str <- ios 
                    return (lines str)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

trd3 :: (a, b, c) -> c
trd3 (_, _, x) = x