module LexrankAPI where 
-- This file assumes there exists a lexrankReceiver js file in the same directory,
-- which further requires there exists directory in the same level, called Lexrank 
-- as well as the readline node module that exists in the directory. 

import System.Process


-- Take in a string of sentences and output a IO string according to salience order 
-- suggested by the Lexrank algorithm. 
lexrankedStrs :: String -> IO String
lexrankedStrs paragraphs = readCreateProcess (shell "node lexrankReceiver.js") (paragraphs ++ "\n")