module QuestionGenerator where 

--import Data.List
--import Data.Char
--import Model
--import P hiding (person)
--import P_Type
--import StringToEntityPredicate
--import DRAC_Type
--import SentEx
--import SentConvert
--import DRAC 
import System.Random
import Control.Monad

-- Deal with Sent 
-- 5 Question types 
-- The type of question we could support based on the current grammar: 
-- 1. What happens if ...?
-- 2. What could made ... happen?

-- For question such as what and who
-- If we neglect the object (object verb subject), 
--    what verb subject? 
--        If the verbs are just be-verbs (is, are), do nothing
--        else, change verb according to tense.

-- If we neglect the verbs in a sent like Object Verb, 
--    What does (change according to tense) object do?  
-- If we neglect the verbs in a sent like Object Verb Subject, 
--    What does (change according to tense) Object do to Subject? 

-- If we neglect the subject (if there's a subject)
-- What does (change according to tense) Object Verb?
--    e.g. He killed the birds. 
--    Q:   What did he kill?

genRandomInt :: (Num a, Random a) => IO a
genRandomInt = randomRIO (1, 4)

--sentToQ :: Sent -> Question 
--sentToQ (IF sent1 sent2) = 