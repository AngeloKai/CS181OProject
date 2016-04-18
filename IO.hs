import Data.Word
import Data.IORef

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
          -- Changed this to getContent if we need to get the content of a file 
          echo 
          --textBlock <- getLine 
          if parse
              then 
                do 
                  putStrLn "Here is question 1: \n"
              else 
                putStrLn "Sorry we can't currently parse this paragraphs. We will support them soon"
          --putStrLn ("You said "++s)
          --putStrLn "Please make another guess"
          --playAgain
      else putStrLn "done"

main :: IO ()
main = playAgain