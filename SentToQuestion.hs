module SentToQuestion where

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

