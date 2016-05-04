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

exParagraph :: String
exParagraph = "IsaacNewton owns a telescope. The telescope helped him study the universe. If a person owns a telescope, the person is a astronomer. Astronomers study stars. Every star shines. Every astronomer loves."

--feedback :: IO ()
--feedback = do 
--  putStrLn "On a scale of 1-5, how difficult do you think the question is?"
--  feedback <- getLine

playAgain :: IO ()
playAgain = 
  do 
    putStrLn "Hi! Thank you for using QA! This program is designed to help students study better by automatically generate quizes based on input paragraphs! You can quit at any second by typing done."
    putStrLn "How many quizes do you want to practice today?"
    quizNum <- getLine 
    putStrLn "Please give me the paragraphs you want to practice with"
    paragraphs <- getLine
    putStrLn "Who did the telescope help? \n"
    putStrLn "A. newton"
    putStrLn "B. albert einstein"
    putStrLn "C. galileo galilei"
    answer1 <- getLine
    putStrLn "What do astronomers study? \n"
    putStrLn "A. planet"
    putStrLn "B. galaxy"
    putStrLn "C. star"
    answer2 <- getLine
    putStrLn "What does Newton own? \n"
    putStrLn "A. scope"
    putStrLn "B. telescope"
    putStrLn "C. microscope"
    answer3 <- getLine 
    putStrLn "Congrats!"

--playAgain :: IO ()
--playAgain = 
--  do 
--    putStrLn "Type a word"
    --s <- getLine 
    --if s /= "done" 
    --  then 
    --    do 
    --      putStrLn ("You said "++s)
    --      putStrLn "Please make another guess"
    --      playAgain
    --  else putStrLn "done"

main :: IO ()
main = playAgain

------ Utilities Functions ------------
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

trd3 :: (a, b, c) -> c
trd3 (_, _, x) = x