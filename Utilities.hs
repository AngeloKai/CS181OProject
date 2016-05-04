module Utilities where 

import Data.List
import Data.Char
import System.Random


-- Check if a string is an integer 
stringIsInt :: String -> Bool
stringIsInt = all isDigit

-- Convert a string to lowercase 
strToLower :: String -> String
strToLower = map toLower

-- Generate random integer between the range 1 to 3
genRandomInt :: (Num a, Random a) => IO a
genRandomInt = randomRIO (1, 4)