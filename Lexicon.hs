module Lexicon where 

import Prelude hiding ((<*>),(<$>))
import Data.List
import Data.Char
import P_Type
import FPH


lexicon :: String -> [Cat]

lexicon "i"   = [Cat "i"   "NP" [Pers,Fst,Sg,Nom, Hum]        []]
lexicon "me"  = [Cat "me"  "NP" [Pers,Fst,Sg,AccOrDat, Hum]   []]
lexicon "we"  = [Cat "we"  "NP" [Pers,Fst,Pl,Nom, Hum]        []]
lexicon "us"  = [Cat "us"  "NP" [Pers,Fst,Pl,AccOrDat, Hum]   []]
lexicon "you" = [Cat "you" "NP" [Pers,Snd, Hum]               []]
lexicon "he"  = [Cat "he"  "NP" [Pers,Thrd,Sg,Nom,Masc, Hum]  []]
lexicon "him" = [Cat "him" "NP" [Pers,Thrd,Sg,AccOrDat,Masc, Hum] 
                                         []]
lexicon "she" = [Cat "she" "NP" [Pers,Thrd,Sg,Nom,Fem, Hum]   []]
lexicon "her" = [Cat "her" "NP" [Pers,Thrd,Sg,AccOrDat,Fem, Hum] 
                                         []]
lexicon "it"  = [Cat "it"  "NP" [Pers,Thrd,Sg,Neutr, Obj]     []]
lexicon "they" = [Cat "they" "NP" [Pers,Thrd,Pl,Nom, Hum]     []]
lexicon "them" = [Cat "them" "NP" [Pers,Thrd,Pl,AccOrDat, Hum] 
                                         []]

lexicon "myself"     = 
 [Cat "myself"     "NP" [Refl,Sg,Fst,AccOrDat, Hum] []]
lexicon "ourselves"  = 
 [Cat "ourselves"  "NP" [Refl,Pl,Fst,AccOrDat, Hum] []]
lexicon "yourself"   = 
 [Cat "yourself"   "NP" [Refl,Sg,Snd,AccOrDat, Hum] []]
lexicon "yourselves" = 
 [Cat "yourselves" "NP" [Refl,Pl,Snd,AccOrDat, Hum] []]
lexicon "himself"    = 
 [Cat "himself"    "NP" [Refl,Sg,Thrd,AccOrDat,Masc, Hum]  []]
lexicon "herself"    = 
 [Cat "herself"    "NP" [Refl,Sg,Thrd,AccOrDat,Fem, Hum]   []]
lexicon "itself"     = 
 [Cat "itself"     "NP" [Refl,Sg,Thrd,AccOrDat,Neutr, Obj] []]
lexicon "themselves" = 
 [Cat "themselves" "NP" [Refl,Pl,Thrd,AccOrDat, Hum] []]

lexicon "who"     = [Cat "who" "NP"  [Wh,Thrd,MascOrFem, Hum] [], 
     Cat "who" "REL" [MascOrFem]         []]
lexicon "whom"    = 
 [Cat "whom" "NP"  [Sg,Wh,Thrd,AccOrDat,MascOrFem, Hum] [], 
  Cat "whom" "REL" [Sg,MascOrFem,AccOrDat, Hum]         []]
lexicon "what"    = 
 [Cat "what" "NP"  [Wh,Thrd,AccOrDat,Neutr]    []]
lexicon "that"    = [Cat "that"  "REL" []      [], 
                     Cat "that"  "DET" [Sg]    []]
lexicon "which"   = [Cat "which" "REL" [Neutr] [], 
                     Cat "which" "DET" [Wh]    []]

lexicon "snowwhite"    = 
 [Cat "snowwhite"  "NP" [Thrd,Fem,Sg, Hum]  []]
lexicon "alice"        = 
 [Cat "alice"      "NP" [Thrd,Fem,Sg, Hum]  []]
lexicon "dorothy"      = 
 [Cat "dorothy"    "NP" [Thrd,Fem,Sg, Hum]  []]
lexicon "goldilocks"   = 
 [Cat "goldilocks" "NP" [Thrd,Fem,Sg, Hum]  []]
lexicon "littlemook"   = 
 [Cat "littlemook" "NP" [Thrd,Masc,Sg, Hum] []]
lexicon "atreyu"       = 
 [Cat "atreyu"     "NP" [Thrd,Masc,Sg, Hum] []]
lexicon "isaacnewton" = 
 [Cat "isaacnewton"    "NP" [Thrd,Masc,Sg, Hum] []]
lexicon "newton"      = 
 [Cat "isaacnewton"    "NP" [Thrd,Masc,Sg, Hum] []]


lexicon "every"   = [Cat "every"   "DET" [Sg]  []]
lexicon "all"     = [Cat "all"     "DET" [Pl]  []]
lexicon "some"    = [Cat "some"    "DET" []    []]
lexicon "several" = [Cat "several" "DET" [Pl]  []]
lexicon "a"       = [Cat "a"       "DET" [Sg]  []]
lexicon "no"      = [Cat "no"      "DET" []    []]
lexicon "the"     = [Cat "the"     "DET" []    []]

lexicon "most"    = [Cat "most"    "DET" [Pl]  []]
lexicon "many"    = [Cat "many"    "DET" [Pl]  []]
lexicon "few"     = [Cat "few"     "DET" [Pl]  []]
lexicon "this"    = [Cat "this"    "DET" [Sg]  []]
lexicon "these"   = [Cat "these"   "DET" [Pl]  []]
lexicon "those"   = [Cat "those"   "DET" [Pl]  []]

lexicon "less_than" = [Cat "less_than" "DF" [Pl] []]
lexicon "more_than" = [Cat "more_than" "DF" [Pl] []]

lexicon "thing"   = [Cat "thing"   "CN" [Sg,Neutr,Thrd, Obj] []]
lexicon "things"  = [Cat "things"  "CN" [Pl,Neutr,Thrd, Obj] []]
lexicon "person"  = [Cat "person"  "CN" [Sg,Masc,Thrd, Hum]  []]
lexicon "persons" = [Cat "persons" "CN" [Pl,Masc,Thrd, Hum]  []]
lexicon "boy"     = [Cat "boy"     "CN" [Sg,Masc,Thrd, Hum]  []]
lexicon "boys"    = [Cat "boys"    "CN" [Pl,Masc,Thrd, Hum]  []]
lexicon "man"     = [Cat "man"     "CN" [Sg,Masc,Thrd, Hum]  []]
lexicon "men"     = [Cat "men"     "CN" [Pl,Masc,Thrd, Hum]  []]
lexicon "girl"    = [Cat "girl"    "CN" [Sg,Fem,Thrd, Hum]   []]
lexicon "girls"   = [Cat "girls"   "CN" [Pl,Fem,Thrd, Hum]   []]
lexicon "woman"   = [Cat "woman"   "CN" [Sg,Fem,Thrd, Hum]   []]
lexicon "women"   = [Cat "women"   "CN" [Pl,Fem,Thrd, Hum]   []]
lexicon "princess" = [Cat "princess" "CN" [Sg,Fem,Thrd, Hum] []]
lexicon "princesses" = [Cat "princesses" "CN" [Pl,Fem,Thrd, Hum] []]
lexicon "dwarf"    = [Cat "dwarf"    "CN" [Sg,Masc,Thrd, Hum] []]
lexicon "dwarfs"   = [Cat "dwarfs"   "CN" [Pl,Masc,Thrd, Hum] []]
lexicon "dwarves"  = [Cat "dwarves"  "CN" [Pl,Masc,Thrd, Hum] []]
lexicon "giant"    = [Cat "giant"    "CN" [Sg,Masc,Thrd, Hum] []]
lexicon "giants"   = [Cat "giants"   "CN" [Pl,Masc,Thrd, Hum] []]

lexicon "wizard"   = [Cat "wizard"   "CN" [Sg,Masc,Thrd, Hum]  []]
lexicon "wizards"  = [Cat "wizards"  "CN" [Pl,Masc,Thrd, Hum]  []]
lexicon "sword"    = [Cat "sword"    "CN" [Sg,Neutr,Thrd, Obj] []]
lexicon "swords"   = [Cat "swords"   "CN" [Pl,Neutr,Thrd, Obj] []]
lexicon "dagger"   = [Cat "dagger"   "CN" [Sg,Neutr,Thrd, Obj] []]
lexicon "daggers"  = [Cat "daggers"  "CN" [Pl,Neutr,Thrd, Obj] []]


lexicon "telescope"   = [Cat "telescope"   "CN" [Sg,Neutr,Thrd, Obj] []]
lexicon "telescopes"  = [Cat "telescopes"   "CN" [Pl,Neutr,Thrd, Obj] []]
lexicon "universe"    = [Cat "universe"   "CN" [Sg,Neutr,Thrd, Obj] []]
lexicon "universes"   = [Cat "universes"   "CN" [Pl,Neutr,Thrd, Obj] []]
lexicon "star"        = [Cat "star"   "CN" [Sg,Neutr,Thrd] []]
lexicon "stars"       = [Cat "stars"   "CN" [Pl,Neutr,Thrd] []]
lexicon "astronomer"  = [Cat "astronomer"   "CN" [Sg,Neutr,Thrd, Hum] []]
lexicon "astronomers" = [Cat "astronomers"   "CN" [Pl,Neutr,Thrd, Hum] []]


lexicon "did"    = [Cat "did"    "AUX" [] []]
lexicon "didn't" = [Cat "didn't" "AUX" [] []]

lexicon "smiled"    = [Cat "smiled"    "VP" [Tense] []]
lexicon "smile"     = [Cat "smile"     "VP" [Tense, Pl]  []]
lexicon "smiles"    = [Cat "smiles"     "VP" [Tense, Sg]  []]

lexicon "laughed"   = [Cat "laughed"   "VP" [Tense] []]
lexicon "laugh"     = [Cat "laugh"     "VP" [Tense, Pl]  []]
lexicon "laughs"    = [Cat "laughs"     "VP" [Tense, Sg]  []]

lexicon "cheered"   = [Cat "cheered"   "VP" [Tense] []]
lexicon "cheer"     = [Cat "cheer"     "VP" [Tense,Pl]  []]
lexicon "cheers"    = [Cat "cheers"     "VP" [Tense, Sg]  []]

lexicon "shuddered" = [Cat "shuddered" "VP" [Tense] []]
lexicon "shudder"   = [Cat "shudder"   "VP" [Tense, Pl]  []]
lexicon "shudders"  = [Cat "shudders"     "VP" [Tense, Sg]  []]

lexicon "shined"    = [Cat "shined"    "VP" [Tense] []]
lexicon "shine"     = [Cat "shine"     "VP" [Tense, Pl]  []]
lexicon "shines"    = [Cat "shines"     "VP" [Tense] []] 

lexicon "loved"        = 
 [Cat "loved"    "VP" [Tense] [Cat "_" "NP" [AccOrDat] []]]
lexicon "love"         = 
 [Cat "love"     "VP" [Tense, Pl]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "loves"         = 
 [Cat "loves"     "VP" [Tense, Sg]  [Cat "_" "NP" [AccOrDat] []]]

lexicon "admired"      = 
 [Cat "admired"  "VP" [Tense] [Cat "_" "NP" [AccOrDat] []]]
lexicon "admire"       = 
 [Cat "admire"   "VP" [Tense, Pl]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "admires"       = 
 [Cat "admires"   "VP" [Tense, Sg]  [Cat "_" "NP" [AccOrDat] []]]

lexicon "owned"      = 
 [Cat "owned"  "VP" [Tense] [Cat "_" "NP" [AccOrDat] []]]
lexicon "own"       = 
 [Cat "own"   "VP" [Tense, Pl]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "owns"       = 
 [Cat "owns"   "VP" [Tense, Sg]  [Cat "_" "NP" [AccOrDat] []]]

lexicon "are"        = 
 [Cat "are"   "VP" [Tense, Pl]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "is"        = 
 [Cat "is"   "VP" [Tense, Sg]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "was"        = 
 [Cat "was"   "VP" [Tense, Sg]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "were"        = 
 [Cat "were"   "VP" [Tense, Sg]  [Cat "_" "NP" [AccOrDat] []]]


lexicon "studied"      = 
 [Cat "studied"  "VP" [Tense] [Cat "_" "NP" [AccOrDat] []]]
lexicon "study"       = 
 [Cat "study"   "VP" [Tense, Pl]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "studies"       = 
 [Cat "studies"   "VP" [Tense, Sg]  [Cat "_" "NP" [AccOrDat] []]]


lexicon "helped"       = 
 [Cat "helped"   "VP" [Tense] [Cat "_" "NP" [AccOrDat] []]]
lexicon "help"         = 
 [Cat "help"     "VP" [Tense, Pl]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "helps"         = 
 [Cat "helps"     "VP" [Tense, Sg]  [Cat "_" "NP" [AccOrDat] []]]

lexicon "defeated"       = 
 [Cat "defeated" "VP" [Tense] [Cat "_" "NP" [AccOrDat] []]]
lexicon "defeat"         = 
 [Cat "defeat"   "VP" [Tense, Pl]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "defeats"         = 
 [Cat "defeats"   "VP" [Tense, Sg]  [Cat "_" "NP" [AccOrDat] []]]

lexicon "gave"         = 
 [Cat "gave" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [To]       []], 
  Cat "gave" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "NP" [AccOrDat]  []]]
lexicon "give"         = 
 [Cat "give" "VP" [Infl, Pl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [To]       []],
  Cat "give" "VP" [Infl, Pl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "NP" [AccOrDat] []]]
lexicon "gives"         = 
 [Cat "gives" "VP" [Infl, Sg]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [To]       []],
  Cat "gives" "VP" [Infl, Sg]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "NP" [AccOrDat] []]]

lexicon "sold" = 
 [Cat "sold" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [To]       []],
  Cat "sold" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "NP" [AccOrDat] []]]
lexicon "sell" = 
 [Cat "sell" "VP" [Infl, Pl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [To]       []],
  Cat "sell" "VP" [Infl, Pl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "NP" [AccOrDat] []]]
lexicon "sells" = 
 [Cat "sells" "VP" [Infl, Sg]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [To]       []],
  Cat "sells" "VP" [Infl, Sg]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "NP" [AccOrDat] []]]

lexicon "kicked" = 
 [Cat "kicked" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
                             Cat "_" "PP" [With]     []], 
  Cat "kicked" "VP" [Tense] [Cat "_" "NP" [AccOrDat] []]]
lexicon "kick" = 
 [Cat "kick"   "VP" [Infl, Pl]  [Cat "_" "NP" [AccOrDat] [],
                             Cat "_" "PP" [With]     []], 
  Cat "kick"   "VP" [Infl, Pl]  [Cat "_" "NP" [AccOrDat] []]] 
lexicon "kicks" = 
 [Cat "kicks"   "VP" [Infl, Sg]  [Cat "_" "NP" [AccOrDat] [],
                             Cat "_" "PP" [With]     []], 
  Cat "kicks"   "VP" [Infl, Sg]  [Cat "_" "NP" [AccOrDat] []]] 

lexicon "took" = 
 [Cat "took" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [From]     []], 
  Cat "took" "VP" [Tense] [Cat "_" "NP" [AccOrDat] []]]
lexicon "take" = 
 [Cat "take" "VP" [Infl, Pl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [From]     []], 
  Cat "take" "VP" [Infl, Pl]  [Cat "_" "NP" [AccOrDat] []]] 
lexicon "takes" = 
 [Cat "takes" "VP" [Infl, Sg]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [From]     []], 
  Cat "takes" "VP" [Infl, Sg]  [Cat "_" "NP" [AccOrDat] []]] 

lexicon "on"   = [Cat "on"   "PREP" [On]   []]
lexicon "with" = [Cat "with" "PREP" [With] []]
lexicon "by"   = [Cat "by"   "PREP" [By]   []]
lexicon "to"   = [Cat "to"   "PREP" [To]   []]
lexicon "from" = [Cat "from" "PREP" [From] []]

lexicon "and"   = [Cat "and"  "CONJ" [] []]
lexicon "."     = [Cat "."    "CONJ" [] []]
lexicon "if"    = [Cat "if"   "COND" [] []]
lexicon "then"  = [Cat "then" "THEN" [] []]

lexicon _ = []