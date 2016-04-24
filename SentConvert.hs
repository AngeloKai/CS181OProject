module SentConvert where 

import Data.List
import Data.Char
import Model
import P hiding (person)
import P_Type
import StringToEntityPredicate
import DRAC_Type

stringToDet :: String -> DET
stringToDet "every" = Every
stringToDet "some"  = Some
stringToDet "no"    = No
stringToDet "the"   = The 

stringToCN :: String -> CN 
stringToCN "girl"     = Girl
stringToCN "boy"      = Boy
stringToCN "princess" = Princess
stringToCN "dwarf"    = Dwarf
stringToCN "giant"    = Giant
stringToCN "wizard"   = Wizard
stringToCN "sword"    = Sword
stringToCN "poison"   = Poison


stringToVP :: String -> VP
stringToVP "laughed" = Laughed
stringToVP "cheered" = Cheered
stringToVP "shuddered" = Shuddered

stringToTV :: String -> TV
stringToTV "loved"    = Loved
stringToTV "admired"  = Admired
stringToTV "helped"   = Helped
stringToTV "defeated" = Defeated

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