-- Entity and Predicat
e
-- Predicate function 
blowupPred :: OnePlacePred -> Idx -> Trans
blowupPred = \ pred i c -> if   pred (lookupIdx c i) 
                           then [c] 
                           else []

blowupPred2 :: TwoPlacePred -> Idx -> Idx -> Trans
blowupPred2 = \ pred i1 i2 c -> 
                      let l1 = lookupIdx c i1
                          l2 = lookupIdx c i2
                      in  if   pred l1 l2
                          then [c] 
                          else []

blowupPred3 :: ThreePlacePred -> Idx -> Idx -> Idx -> Trans
blowupPred3 = \ pred i1 i2 i3 c -> 
                         let l1 = lookupIdx c i1
                             l2 = lookupIdx c i2
                             l3 = lookupIdx c i3
                         in  if   pred l1 l2 l3 
                                  then [c] 
                                  else []

name2entity :: String -> Entity
name2entity "snowwhite"  = snowWhite
name2entity "alice"      = alice 
name2entity "dorothy"    = dorothy 
name2entity "goldilocks" = goldilocks 
name2entity "littlemook" = littleMook
name2entity "atreyu"     = atreyu 

name2pred :: String -> OnePlacePred
name2pred "laugh"     = laugh
name2pred "laughed"   = laugh
name2pred "cheer"     = cheer
name2pred "cheered"   = cheer
name2pred "shudder"   = shudder 
name2pred "shuddered" = shudder 

name2pred "thing"      = thing 
name2pred "things"     = thing
name2pred "person"     = person
name2pred "persons"    = person
name2pred "boy"        = boy 
name2pred "boys"       = boy 
name2pred "man"        = man
name2pred "men"        = man
name2pred "girl"       = girl
name2pred "girls"      = girl
name2pred "woman"      = woman
name2pred "women"      = woman
name2pred "princess"   = princess
name2pred "princesses" = princess

name2pred "dwarf"      = dwarf
name2pred "dwarfs"     = dwarf
name2pred "dwarves"    = dwarf
name2pred "giant"      = giant
name2pred "giants"     = giant
name2pred "wizard"     = wizard
name2pred "wizards"    = wizard
name2pred "sword"      = sword
name2pred "swords"     = sword
name2pred "dagger"     = dagger
name2pred "daggers"    = dagger

name2binpred :: String -> TwoPlacePred
name2binpred "love"     = love
name2binpred "loved"    = love
name2binpred "admire"   = admire
name2binpred "admired"  = admire
name2binpred "help"     = help 
name2binpred "helped"   = help 
name2binpred "defeat"   = defeat
name2binpred "defeated" = defeat

name2terpred :: String -> ThreePlacePred
name2terpred "give" = give 
name2terpred "gave" = give 