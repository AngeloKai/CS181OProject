import System.Process


main :: IO ()
main = 
    do 
        putStrLn "Give me the paragraphs \n"
        paragraphs <- getLine
        output <- readCreateProcess (shell "node lexrankReceiver.js") (paragraphs ++ "\n")
        putStrLn output