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

sum1 :: Int -> Int 
sum1 x = x + 1

sum2 :: Int -> Int 
sum2 x = x * 2

add :: Int -> Int
add = sum2 . sum1 

-- Check if a string is an integer 
stringIsInt :: String -> Bool
stringIsInt = all isDigit

strToLower :: String -> String
strToLower = map toLower


parseEval :: String -> Prop'
parseEval = eval' . treeToSent . parses

--[
--[.S[Thrd,Fem,Sg] 
--[alice NP[Fem,Sg,Thrd,Nom],
--[.VP[Tense] [admired VP[Tense],dorothy NP[Thrd,Fem,Sg]]]
--]
--]]
-- = [Branch "S"
--  [Branch "NP" [Leaf "alice"],
--  Branch "VP" [Branch "TV" [Leaf "admired"],
--              [Branch "NP" [Leaf "dorothy"]]]
--  ]
--  ]

-- snowwhite3 :: ParseTree Cat Cat 
-- snowwhite3 = Branch (Cat "_" "S" [] []) [ Leaf (Cat "snowwhite" "NP" [Thrd, Fem, Sg] []), 
-- Branch (Cat "_" "VP" [Tense] [] ) [Leaf (Cat "laughed"   "VP" [Tense] [])] ]


treeToSent :: [ParseTree Cat Cat] -> Sent
treeToSent [Branch (Cat _ "S" ls _ ) rest ] = treeToSent rest
treeToSent [Leaf (Cat np "NP" ls catArray ), Branch (Cat _ "VP" ls2 _ ) vp ] = 
  Sent (treeToNP [Leaf (Cat np "NP" ls catArray )] ) (treeToVP vp)
treeToSent [ Branch (Cat _ "NP" ls _ ) np , Branch (Cat __ "VP" ls2 _ ) vp ] =
  Sent (treeToNP np) (treeToVP vp)

treeToNP :: [ParseTree Cat Cat] -> NP
treeToNP [ Leaf (Cat npPhon "NP" ls _ ) ] =
  if (stringIsInt npPhon) 
    then   PRO (read npPhon)
    else   case (strToLower npPhon) of "snowwhite"   -> SnowWhite
                                       "alice"       -> Alice
                                       "dorothy"     -> Dorothy
                                       "goldilocks"  -> Goldilocks
                                       "littlemook"  -> LittleMook
                                       "atreyu"      -> Atreyu
                                       "he"          -> He
                                       "she"         -> She
                                       "it"          -> It
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
treeToVP [ Leaf (Cat vp "VP" ls []) ] = (stringToVP vp)
treeToVP [ Leaf (Cat vp "VP" ls []), Leaf (Cat npPhon "NP" attrs catArr)] = 
  if (any (Refl ==) attrs) 
    then VP2 (stringToTV (strToLower vp)) (stringToREFL npPhon)
    else VP1 (stringToTV (strToLower vp)) (treeToNP [Leaf (Cat npPhon "NP" attrs catArr)])
treeToVP [ Leaf (Cat vp "VP" ls []), Branch (Cat npPhon "NP" attrs catArr) rest ] = 
  VP1 (stringToTV (strToLower vp)) (treeToNP [Branch (Cat npPhon "NP" attrs catArr) rest])
-- treeToVP [ Leaf (Cat tv "TV" ls []), vp] = VP1 (stringToTV tv) (treeToNP [vp])
-- treeToVP [ Branch (Cat "_" "VP" _ _) rest] = treeToVP rest
treeToVP ( Leaf (Cat auxPhon "AUX" _ _) : rest) = VP5 (stringToAUX auxPhon) (treeToINF rest)


-- No need to worry about case issue since verbs are suppposed to be lower
-- case in this grammar. 
treeToINF :: [ParseTree Cat Cat] -> INF 
treeToINF [Branch vpSign [Leaf (Cat infPhon _ _ _)]] = (stringToINF infPhon) 
treeToINF [Branch vpSign [Leaf (Cat tinfPhon _ _ _), np1]] = INF1 (stringToTINF tinfPhon) (treeToNP [np1]) 
treeToINF [Branch vpSign [Leaf (Cat "give" _ _ _), np1, np2]] = INF2 Give (treeToNP [np1]) (treeToNP [np2])




-- treeToVP :: [ParseTree Cat Cat] -> VP
-- treeToVP [ Leaf (Cat vp "VP" _ _) ] = vp
-- treeToVP [ Leaf (Cat tv "TV" _ _), vp] = VP1 tv (treeToNP vp)



--treeToVP :: ParseTree Cat Cat -> VP
--treeToVP (Leaf vp) = vp
--treeToVP (Branch "TV" [Leaf tv], np) = VP1 tv (treeToNP np)
--treeToVP (Branch "TV" [Leaf tv], Branch "REFL" [Leaf refl]) = VP2 tv refl


-- treeToSent :: [ParseTree Cat Cat] -> Sent
-- treeToSent [Branch "S" rest ] = Sent treeToSent rest
-- treeToSent [Branch "NP" np, Branch "VP" vp] = Sent (treeToNP np) (treeToVP vp)


-- treeToSent [Branch "NP" [Leaf np], Branch "VP" [Branch "TV" [Leaf tv], [Branch "NP" [Leaf ]]]]
-- treeToSent [Branch "NP" [ParseTree a b]] = Sent np (treeToSent [ParseTree a b])
-- treeToSent [Ep] = Sent NULL NULL
-- treeToSent [Leaf a] = Sent a NULL



--data Sent = Sent NP VP | If Sent Sent | Txt Sent Sent
--          deriving (Eq,Show)
--data NP   = SnowWhite  | Alice | Dorothy | Goldilocks 
--          | LittleMook | Atreyu
--          | PRO Idx    | He | She | It
--          | NP1 DET CN | NP2 DET RCN 
--          deriving (Eq,Show)
--data DET  = Every | Some | No | The 
--          deriving (Eq,Show)
--data CN   = Girl   | Boy    | Princess | Dwarf | Giant 
--          | Wizard | Sword  | Poison 
--          deriving (Eq,Show) 
--data RCN  = RCN1 CN That VP | RCN2 CN That NP TV
--          deriving (Eq,Show)
--data That = That deriving (Eq,Show)
--data REFL = Self deriving (Eq,Show)

--data VP   = Laughed | Cheered | Shuddered 
--          | VP1 TV NP    | VP2 TV REFL 
--          | VP3 DV NP NP | VP4 DV REFL NP 
--          | VP5 AUX INF  
--          deriving (Eq,Show) 
--data TV   = Loved   | Admired | Helped | Defeated
--          deriving (Eq,Show)
--data DV   = Gave deriving (Eq,Show)

--data AUX  = DidNot deriving (Eq,Show) 

--data INF  = Laugh | Cheer  | Shudder 
--          | INF1  TINF NP  | INF2  DINF NP NP 
--          deriving (Eq,Show) 
--data TINF = Love  | Admire | Help | Defeat 
--          deriving (Eq,Show) 
--data DINF = Give deriving (Eq,Show) 

--treeToSent [Branch "NP" rest] = treeToSent rest

--data Sent = Sent NP VP | If Sent Sent | Txt Sent Sent
--          deriving (Eq,Show)

-- eval' :: Sent -> Prop'
-- eval' s = intS' s (convert context) True


-- Parses can parse both single sentence or a block sentence" 
-- parses :: String -> [ParseTree Cat Cat]
-- parses str = let ws = lexer str 
--              in  [ s | catlist   <- collectCats lexicon ws, 
--                        (s,[],[]) <- prsTXT [] catlist  
--                                  ++ prsYN  [] catlist   
--                                  ++ prsWH  [] catlist ]