module SentConvert where 

import Data.List
import Data.Char
import Model
import P hiding (person)
import P_Type
import StringToEntityPredicate
import DRAC_Type

stringToNP :: String -> NP
stringToNP "snowwhite"   = SnowWhite
stringToNP "alice"       = Alice
stringToNP "dorothy"     = Dorothy
stringToNP "goldilocks"  = Goldilocks
stringToNP "littlemook"  = LittleMook
stringToNP "atreyu"      = Atreyu
stringToNP "he"          = He
stringToNP "she"         = She
stringToNP "it"          = It
stringToNP "newton"      = IsaacNewton 
stringToNP "Newton"      = IsaacNewton
stringToNP "isaacnewton" = IsaacNewton
stringToNP "him"         = Him


-- The isInfixOf function takes two lists and returns True iff the first list is contained, wholly and intact, anywhere within the second.
stringToREFL :: String -> REFL
stringToREFL str = if (isInfixOf "self" str)
                    then Self
                    else error (str ++ " is not a self reference")

stringToDET :: String -> DET
stringToDET "every" = Every
stringToDET "some"  = Some
stringToDET "no"    = No
stringToDET "the"   = The 
stringToDET "a"     = An
stringToDET "an"    = An

stringToCN :: String -> CN 
stringToCN "girl"     = Girl
stringToCN "boy"      = Boy
stringToCN "princess" = Princess
stringToCN "dwarf"    = Dwarf
stringToCN "giant"    = Giant
stringToCN "wizard"   = Wizard
stringToCN "sword"    = Sword
stringToCN "poison"   = Poison
stringToCN "telescope" = Telescope
stringToCN "universe" = Universe
stringToCN "star"     = Star
stringToCN "person"   = Person
stringToCN "astronomer" = Astronomer 

stringToVP :: String -> VP
stringToVP "laughed" = Laughed
stringToVP "cheered" = Cheered
stringToVP "shuddered" = Shuddered
stringToVP "shined"  = Shined 


stringToTV :: String -> TV
stringToTV "loved"    = Loved
stringToTV "admired"  = Admired
stringToTV "helped"   = Helped
stringToTV "defeated" = Defeated
stringToTV "owned"    = Owned
stringToTV "is"       = Is
stringToTV "are"      = Are
stringToTV "was"      = Was
stringToTV "were"      = Were


stringToAUX :: String -> AUX
stringToAUX "didn't" =  DidNot

stringToDV :: String -> DV
stringToDV "gave" = Gave

stringToINF :: String -> INF
stringToINF "laugh"   = Laugh
stringToINF "cheer"   = Cheer
stringToINF "shudder" = Shudder

stringToTINF :: String -> TINF 
stringToTINF "love"   = Love
stringToTINF "admire" = Admire
stringToTINF "help"   = Help 
stringToTINF "defeat" = Defeat

stringToDINF :: String -> DINF
stringToDINF "give" = Give