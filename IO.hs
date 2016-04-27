import Data.Word
import Data.IORef
import Data.List
import Data.Char
import Model
import P hiding (person)
import P_Type
import StringToEntityPredicate
import DRAC_Type
import SentEx
import SentConvert
import DRAC 
import StrToPropPrime
import System.Process
import System.FilePath

echo :: IO()
echo = do str <- getLine
          putStrLn str

ask :: String -> String -> IO()

ask prompt ansPrefix = do putStr (prompt++": ")
                          response <- getLine
                          putStrLn (ansPrefix ++ " " ++ response)

getInteger :: IO Integer  -- type is necessary as read is ambiguous
getInteger = do putStr "Enter an integer: "
                line <- getLine
                return (read line)  -- converts string to int then to IO Integer

parse :: Bool
parse = True 

feedback :: IO ()
feedback = do 
  putStrLn "On a scale of 1-5, how difficult do you think the question is?"
  feedback <- getLine

playAgain :: IO ()
playAgain = 
  do 
    putStrLn "Hi! Thank you for using QA! This program is designed to help students study better by automatically generate quizes based on input paragraphs! You can quit at any second by typing done."
    putStrLn "How many quizes do you want to practice today?"
    quizNum <- getLine 
    if quizNum /= "done" 
      then 
        do 
          putStrLn "Please give me the paragraphs you want to practice with"
          paragraphs <- getLine
          putStrLn "Who did the telescope help? \n"
          putStrLn "A. newton \n"
          putStrLn "B. albert einstein \n"
          putStrLn "C. galileo galilei \n"
          answer1 <- getLine
      --    if paragraphs /= "done"
      --      then 
      --        do 
      --          putStrLn "Who did the telescope help? \n"
      --          putStrLn "A. newton \n"
      --          putStrLn "B. albert einstein \n"
      --          putStrLn "C. galileo galilei \n"
      --          answer1 <- getLine
      --        if answer1 == "A"
      --          then
      --            do
      --              putStrLn "What do astronomers study? \n"
      --              putStrLn "A. planet"
      --              putStrLn "B. galaxy"
      --              putStrLn "C. star"
      --              answer2 <- getLine
      --            if answer2 == "C"
      --              then 
      --                do
      --                  putStrLn "What does Newton own? \n"
      --                  putStrLn "A. scope"
      --                  putStrLn "B. telescope"
      --                  putStrLn "C. microscope"
      --                  answer3 <- getLine  
      --                if answer3 == "B"
      --                  then
      --                    putStrLn "Congrats!"
      --              else 
      --                putStrLn "done"
                      
      --          else 
      --            putStrLn "done"
      --      else 
      --        putStrLn "done"
      --else putStrLn "done"

          -- Changed this to getContent if we need to get the content of a file 
          -- The arrow takes the string out from IO String
          --paragraph <- getLine 
          ----putStrLn paragraph
          --lexrankOutput <- (readCreateProcessWithExitCode (shell ("node ./lexrank/try.js " ++ "\"" ++ paragraph ++ "\"")) "")
          ----putStrLn (fst3 lexrankOutput)
          --putStrLn (snd3 lexrankOutput)
          --putStrLn (trd3 lexrankOutput)
          -- The directory name seems to be platform independent. 
          --if ((parseEval paragraph) /= [])
          --    then 
          --      do 

          --        putStrLn "Bye"
          --        --if (exitCode == ExitSuccess) {
          --        --  putStrLn "Bye"
          --        --}
          --        --else {
          --        --  putStrLn "Error with lexrank"
          --        --}
          --    else 
          --      putStrLn "Sorry we can't currently parse this paragraphs. We will support them soon"
          --putStrLn ("You said "++s)
          --putStrLn "Please make another guess"
          --playAgain

main :: IO ()
main = playAgain

------ Utilities Functions ------------
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

trd3 :: (a, b, c) -> c
trd3 (_, _, x) = x