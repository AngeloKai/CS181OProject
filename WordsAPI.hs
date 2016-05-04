-- This file requires python 2.6

module WordsAPI where 

import System.Process


-- Take in a string of a word and output a IO string according to Words API
getInstanceOf :: String -> Integer -> IO String
getInstanceOf word int = readCreateProcess (shell ("python .\\WordsAPI-python\\getInstanceOf.py " ++ word ++ " " ++ (show int))) ""  

-- Take in a string of a word and output a IO string according to Words API
getSimilarTo :: String -> Integer -> IO String
getSimilarTo word int = readCreateProcess (shell ("python .\\WordsAPI-python\\getSimilarTo.py " ++ word ++ " " ++ (show int))) "" 

-- Take in a string of a word and output a IO string according to Words API
getSynonyms :: String -> Integer -> IO String
getSynonyms word int = readCreateProcess (shell ("python .\\WordsAPI-python\\getSynonyms.py " ++ word ++ " " ++ (show int))) "" 

hasInstanceOf :: String -> Integer -> IO [String]
hasInstanceOf word int = iostrToArr response
    where response = readCreateProcess (shell ("python .\\WordsAPI-python\\hasInstanceOf.py " ++ word ++ " " ++ (show int))) "" 


iostrToArr :: IO String -> IO [String]
iostrToArr ios = do str <- ios 
                    return (lines str)