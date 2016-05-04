module StrToPropPrime where

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

-- SnowWhite laughed. No dwarf admired some princess that 
-- shuddered. Every girl that some boy loved cheered. The
-- wizard that helped Snow White defeated the giant. 

parseEval :: String -> Prop'
parseEval = eval' . treeToSent . parses

sentParse :: String -> Sent 
sentParse = treeToSent . parses

treeToSent :: [ParseTree Cat Cat] -> Sent
treeToSent [Branch (Cat _ "S" _ _ ) rest ] = treeToSent rest
treeToSent [npOfSent, vpOfSent ] = 
  Sent (treeToNP [npOfSent] ) (treeToVP [vpOfSent])
treeToSent [Leaf (Cat "if" "COND" _ _), sent1, sent2] = If (treeToSent [sent1]) (treeToSent [sent2])
treeToSent [Branch (Cat _ "TXT" _ _) [sent1, conj, sent2]] = Txt (treeToSent [sent1]) (treeToSent [sent2])
-- treeToSent [ Branch (Cat _ "NP" ls _ ) np , Branch (Cat __ "VP" ls2 _ ) vp ] =
--   Sent (treeToNP np) (treeToVP vp)


treeToNP :: [ParseTree Cat Cat] -> NP
treeToNP [Branch (Cat "_" "NP" _ _) rest] = treeToNP rest
treeToNP [ Leaf (Cat npPhon "NP" _ _ ) ] =
  if (stringIsInt npPhon) 
    then   PRO (read npPhon)
    else   stringToNP (strToLower npPhon)
    --else   case (strToLower npPhon) of "snowwhite"   -> SnowWhite
    --                                   "alice"       -> Alice
    --                                   "dorothy"     -> Dorothy
    --                                   "goldilocks"  -> Goldilocks
    --                                   "littlemook"  -> LittleMook
    --                                   "atreyu"      -> Atreyu
    --                                   "he"          -> He
    --                                   "she"         -> She
    --                                   "it"          -> It
treeToNP [ Leaf (Cat det "DET" _ _ ), Leaf (Cat cnPhon "CN" _ _) ] = NP1 (stringToDET (strToLower det)) (stringToCN (strToLower cnPhon))
treeToNP [ Leaf (Cat det "DET" _ _ ), Branch (Cat cnPhon "CN" _ _) rcn ] = NP2 (stringToDET (strToLower det)) (treeToRCN rcn)

-- Even though the parseTree has a S tag, the tree cannot be parsed by treeToSent 
-- because a Sent can only have one Sent tag 
treeToRCN :: [ParseTree Cat Cat] -> RCN
treeToRCN [Leaf (Cat cnPhon "CN" _ _), Branch (Cat _ "COMP" _ _) [Leaf (Cat "that" "REL" _ _), Branch (Cat _ "S" _ _) [Leaf (Cat "#" "NP" _ _), vpTree]]] = RCN1 (stringToCN (strToLower cnPhon)) That (treeToVP [vpTree])
treeToRCN [Leaf (Cat cnPhon "CN" _ _), Branch (Cat _ "COMP" _ _) [Leaf (Cat "that" "REL" _ _), Branch (Cat _ "S" _ _) [Leaf (Cat np "NP" ls catArr ), Branch (Cat _ "VP" ls2 _ ) tvTree ]]] = 
  RCN2 (stringToCN (strToLower cnPhon)) That (treeToNP [Leaf (Cat np "NP" ls catArr )] ) (treeToTV tvTree)
treeToRCN [Leaf (Cat cnPhon "CN" _ _), Branch (Cat _ "COMP" _ _) [Leaf (Cat "that" "REL" _ _), Branch (Cat _ "S" _ _) [ Branch (Cat _ "NP" ls _ ) np , Branch (Cat _ "VP" ls2 _ ) tvTree ]]] = 
  RCN2 (stringToCN (strToLower cnPhon)) That (treeToNP np) (treeToTV tvTree)

treeToTV :: [ParseTree Cat Cat] -> TV
treeToTV [Leaf (Cat tvPhon "VP" _ _), Leaf (Cat "#" "NP" _ _)] = stringToTV (strToLower tvPhon)


treeToVP :: [ParseTree Cat Cat] -> VP
treeToVP [ Branch (Cat "_" "VP" _ _) rest] = treeToVP rest
treeToVP [ Leaf (Cat vp "VP" _ _) ] = (stringToVP vp)
treeToVP [ Leaf (Cat vp "VP" _ _), Leaf (Cat npPhon "NP" attrs catArr)] = 
  if (any (Refl ==) attrs) 
    then VP2 (stringToTV (strToLower vp)) (stringToREFL npPhon)
    else VP1 (stringToTV (strToLower vp)) (treeToNP [Leaf (Cat npPhon "NP" attrs catArr)])
treeToVP [ Leaf (Cat vp "VP" _ _), Branch (Cat npPhon "NP" attrs catArr) rest ] = 
  VP1 (stringToTV (strToLower vp)) (treeToNP [Branch (Cat npPhon "NP" attrs catArr) rest])
treeToVP [Leaf (Cat vp "VP" _ _), Leaf (Cat npPhon "NP" attrs catArr), np2] = 
  if (any (Refl ==) attrs) 
    then VP4 (stringToDV vp) (stringToREFL npPhon) (treeToNP [np2])
    else VP3 (stringToDV vp) (treeToNP [Leaf (Cat npPhon "NP" attrs catArr)]) (treeToNP [np2])
treeToVP [Leaf (Cat vp "VP" _ _), np1, np2] = VP3 (stringToDV vp) (treeToNP [np1]) (treeToNP [np2])
treeToVP ( Leaf (Cat auxPhon "AUX" _ _) : rest) = VP5 (stringToAUX auxPhon) (treeToINF rest)

-- No need to worry about case issue since verbs are suppposed to be lower
-- case in this grammar. 
treeToINF :: [ParseTree Cat Cat] -> INF 
treeToINF [Branch vpSign [Leaf (Cat infPhon _ _ _)]] = (stringToINF infPhon) 
treeToINF [Branch vpSign [Leaf (Cat tinfPhon _ _ _), np1]] = INF1 (stringToTINF tinfPhon) (treeToNP [np1]) 
treeToINF [Branch vpSign [Leaf (Cat "give" _ _ _), np1, np2]] = INF2 Give (treeToNP [np1]) (treeToNP [np2])


-------- Utilities Functions ----------
-- Check if a string is an integer 
stringIsInt :: String -> Bool
stringIsInt = all isDigit

-- Convert a string to lowercase 
strToLower :: String -> String
strToLower = map toLower