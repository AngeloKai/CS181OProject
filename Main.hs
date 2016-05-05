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
import Utilities
import Lexicon
import System.Process
import System.FilePath
import LexrankAPI
import QuestionGenerator 
import WordsAPI
import System.IO.Unsafe



exParagraph :: String
exParagraph = sent1 ++ sent2 ++ sent3 ++ sent4

sent1, sent2, sent3, sent4:: String 
sent1 = "Newton owned a telescope. "
sent2 = "The telescope helped Newton. "
sent3 = "If a person owned a telescope, he was a astronomer. "
-- Of course, Newton can't admire himself.
sent4 = "Some astronomer admired Newton. "

feedback :: IO ()
feedback = do 
  putStrLn "On a scale of 1-5, how difficult do you think the question is?"
  feedback <- getLine
  putStrLn "Thank you"

distractorGenerator :: String -> Integer -> (String, String)
distractorGenerator str int = (result !! 0, result !! 1)
                              where result = unsafePerformIO (usableDistractor str int) 

distractorNum :: Integer
distractorNum = 2

fullQuestion :: [(String, String)] -> [(String, String, String, String)] 
fullQuestion [] = []
fullQuestion ((question, answer):rest) = [(question, answer, fstDistractor, sndDistractor)] ++ (fullQuestion rest)
                                         where distractor = distractorGenerator answer distractorNum
                                               fstDistractor = fst distractor
                                               sndDistractor = snd distractor

finalQ :: String -> [(String, String, String, String)] 
finalQ str = fullQuestion (questionGenerator str)

question :: [(String, String, String, String)] -> Integer -> IO ()
question ((x1, x2, x3, x4):xs) int = 
    if int == 0 
        then
            do 
                putStrLn "Thank you. That is all the question."
        else 
            do 
                putStrLn ("A. " ++ x1)
                putStrLn ("B. " ++ x2)
                putStrLn ("C. " ++ x3)
                putStrLn ("D. " ++ x4) 
                answer <- getLine
                if answer == "B. "
                    then putStrLn "Well done!"
                    else putStrLn "Sorry, the answer is incorrect"
                feedback 
                question xs (int - 1)
       

main :: IO ()
main = 
  do 
    putStrLn "Hi! Thank you for using QA! This program is designed to help students study better by automatically generate quizes based on input paragraphs! Please note that the current program is undeterministic in terms of the kind of questions that would be generated. It is recommended that you test it multiple times."
    putStrLn "How many quizes do you want to practice today? Please choose 4 for the current version."
    quizNum <- getLine 
    putStrLn ("Please give me the paragraphs you want to practice with. Please copy and paste the following paragraphs: " ++ exParagraph)
    paragraphs <- getLine
    lexranked <- lexrankedStrs paragraphs
    question (finalQ lexranked) (read quizNum)
    putStrLn "Congrats!"