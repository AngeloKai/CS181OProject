-- This file requires python 2.6

module WordsAPI where 

import System.Process
import Utilities 


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

usableDistractor :: String -> Integer -> IO [String]
usableDistractor str int = 
    if str == "isaacnewton" 
        then 
            do 
                instanceResult <- getInstanceOf "Isaac Newton" 1
                hasInstanceOf instanceResult int
        else if str == "Every astronomer"
            then 
                do 
                    return ["physicist", "mathmatician"]
            else if (length (words str) > 1)
                    then 
                        do 
                            return ["He was a mathmatician", "He was a scientist"]
                    else if (str == "Admired")
                            then 
                                do 
                                    return ["loved", "believed"]
                            else if (str == "Helped")
                                then 
                                    do 
                                        return ["assisted", "aided"]
                                else if (str == "Owned")
                                    then 
                                        do 
                                            return ["Have", "Bought"]
                                    else
                                        do 
                                            synonymsResult <- getSynonyms str int
                                            iostrToArr (return synonymsResult)