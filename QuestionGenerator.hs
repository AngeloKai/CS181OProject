module QuestionGenerator where 

import Data.List
import Data.Char
import Model
import P hiding (person)
import P_Type
import StringToEntityPredicate
import DRAC_Type
import DRAC 
import SentEx
import SentConvert
import StrToPropPrime
import Control.Monad
import Utilities
import Lexicon
import System.IO.Unsafe

-- Beause of the monumental work involved, this demo only focused on the
-- following identities:
-- Newton: B 
-- Telescope: Z
-- Astronomer: B, P, M, Y

--Question generation follows the algorithm below:
--
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

questionGenerator :: String -> [(String, String)]
questionGenerator str = prettyPrint (sentToQuestion (sentParse str) (parseEval str))

prettyPrint :: [(String, String)] -> [(String, String)]
prettyPrint [] = []
prettyPrint ((str1, str2):xs) = [(question, answer)] ++ prettyPrint xs 
                                where tag = ["Sent", "NP1", "VP1", "(", ")", "(NP1", "(VP1"]
                                      question = fstLetterUpper (strToLower remaining)
                                      remaining = deleteAllInstances ')' (deleteAllInstances '(' (removeWords str1 tag))
                                      remainingA = deleteAllInstances ')' (deleteAllInstances '(' (removeWords str2 tag))
                                      answer = fstLetterUpper (strToLower remainingA)


sentToQuestion :: Sent -> Prop' -> [(String, String)]
sentToQuestion (Txt sent1 rest@(Txt sent2 sent3)) prop = [questionG sent1 prop] ++ (sentToQuestion rest prop)
sentToQuestion (Txt sent1 sent2) prop = [questionG sent1 prop] ++ [questionG sent2 prop]


questionG :: Sent -> Prop' -> (String, String)
questionG (If sent1 sent2) prop = (("What happens if " ++ (show sent1)), show sent2)  
questionG others prop = randomQuestion others genRandomInt prop


randomQuestion :: Sent -> IO Integer -> Prop' -> (String, String)
randomQuestion sent1@(Sent np1 (VP1 tv np2)) randomInt prop = 
   case unsafePerformIO randomInt of 1 -> objQuestion sent1 prop
                                     2 -> if tv == Was 
                                            then objQuestion sent1 prop
                                            else verbQuestion sent1 prop
                                     3 -> subjQuestion sent1 prop


objQuestion :: Sent -> Prop' -> (String, String)
objQuestion (Sent np1 (VP1 tv np2)) prop = 
    if (np1 == IsaacNewton)
        then (("Who " ++ show tv ++ " " ++ show np2), show (findObj prop tv))
        else (("What " ++ show tv ++ " " ++ show np2), show (findObj prop tv))

-- Need to add a tense checker. But the current lexicon doesn't include
-- this attribute. Adding the attributes takes up too much time
verbQuestion :: Sent -> Prop' -> (String, String)
verbQuestion (Sent np1 (VP1 tv np2)) prop = (("What did " ++ (show np1) ++ " do to " ++ (show (findSubj prop tv))), show tv)

subjQuestion :: Sent -> Prop' -> (String, String)
subjQuestion (Sent np1 (VP1 tv np2)) prop = 
    if (np2 == IsaacNewton)
        then (("Who did " ++ (show (findObj prop tv)) ++ " " ++ (show tv)), show (findSubj prop tv))
        else (("What did " ++ (show (findObj prop tv)) ++ " " ++ (show tv)), show (findSubj prop tv))



findObj :: Prop' -> TV -> NP 
findObj prop tvIdent = findWord (objIdent prop tvIdent)

findSubj :: Prop' -> TV -> NP
findSubj prop tvIdent = findWord (objIdent prop tvIdent)


findWord :: [Entity] -> NP
findWord [B] = IsaacNewton
findWord [Z] = NP1 The Telescope
findWord rest = NP1 Every Astronomer


objIdent :: Prop' -> TV -> [Entity]
objIdent [] tvAsked = []
objIdent ((context,constraintList):rest) tvAsked = [entity] ++ (objIdent rest tvAsked)
  where (idx1, idx2) = idxTuple constraintList tvAsked 
        entity = findIdent context idx1

subIdent :: Prop' -> TV -> [Entity]
subIdent [] tvAsked = []
subIdent ((context,constraintList):rest) tvAsked = [entity] ++ (subIdent rest tvAsked)
  where (idx1, idx2) = idxTuple constraintList tvAsked 
        entity = findIdent context idx2

idxTuple :: [Constraint] -> TV -> (Idx, Idx)
idxTuple ((C2 tv idx1 idx2):xs) tvAsked = if tvAsked == tv 
                                            then (idx1, idx2)
                                            else idxTuple xs tvAsked
idxTuple ((C5 tv idx1 idx2):xs) tvAsked = if tvAsked == tv 
                                            then (idx1, idx2)
                                            else idxTuple xs tvAsked

-- Assume each number matches a unique entity 
findIdent :: [(Idx, Entity)] -> Idx -> Entity 
findIdent ((idx, entity):rest) idxAsked = if idxAsked == idx 
                                            then entity
                                            else findIdent rest idxAsked


----- Utilities Functions ----------------------

hum :: Feat 
hum = Hum 


npIsHuman :: NP -> Bool
npIsHuman (NP1 det cn) = elem hum catAgreement 
    where str = show cn 
          catAgreement = catAgree (lexicon str) 
npIsHuman (NP2 det rcn) = False
npIsHuman rest = elem hum catAgreement 
    where str = show rest 
          catAgreement = catAgree (lexicon str) 

catAgree :: [Cat] -> [Feat]
catAgree [Cat phon catlabel agreement catList] = agreement