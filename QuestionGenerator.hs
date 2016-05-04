module QuestionGenerator where 

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
import Control.Monad
import Utilities

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


--Consider a sentence sent1 = NP1 (VP1 TV NP2)
--E.g. The man killed the bird 
--Remove NP1
--What TV NP2?
--Remove TV
--What does NP1 do to NP2? 
--Remove NP2 
--What does NP1 TV?
--Consider a sentence sent2 = If SentA SentB
--What happens if SentA? 


sentToQuestion :: Sent -> [String]

sentToQuestion Sent np "Laughed" = ["Who laughed?", np]
sentToQuestion Sent np "Cheered" = ["Who cheered?", np]
sentToQuestion Sent np "Shuddered" = ["Who shuddered?", np]

sentToQuestion Sent np (VP1 TV)



----- Utilities Functions ----------------------

--isHuman :: NP -> Bool
--isHuman np = 
--	let 
--		phonNP = phon np
--  	    lowerNp = strToLower phonNP


--fs :: Cat -> Agreement 
--fs (Cat _ _ agr _) = agr

--phon :: Cat -> String
--phon (Cat ph _ _ _) = ph

--sentToQ :: Sent -> Question 
--sentToQ (IF sent1 sent2) = 