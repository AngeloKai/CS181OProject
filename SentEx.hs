module SentEx where 

import Data.List
import Model
import P hiding (person)
import P_Type
import StringToEntityPredicate
import DRAC_Type







ex1  = Sent Dorothy Cheered
ex2  = Sent Dorothy Laughed
ex3  = Sent Dorothy (VP5 DidNot Laugh)
ex4  = Txt  (Sent Dorothy Cheered)
            (Sent LittleMook Cheered)
ex5  = Txt  (Sent Dorothy Cheered)
            (Sent (PRO 1) (VP1 Admired (NP1 Some Girl)))
ex6  = Sent (NP1 Some Boy) (VP1 Loved (NP1 Some Princess))
ex7  = Sent (NP1 Some Boy) (VP1 Loved (NP1 The Princess))

ex8  = Sent (NP1 Some Boy) (VP1 Defeated (NP1 No Giant))
ex9  = Sent (NP1 The  Boy) (VP1 Defeated (NP1 No Giant))
ex10 = Sent (NP1 Some Boy) (VP1 Loved (NP1 The Princess))

ex11 = Sent (NP1 No   Boy) (VP1 Loved Goldilocks)
ex12 = Sent (NP1 Some Boy) (VP1 Loved SnowWhite)
ex13 = Sent LittleMook (VP1 Loved    (NP1 Some Princess))
ex14 = Sent LittleMook (VP1 Defeated (NP2 Some (RCN1 Giant 
                                That (VP1 Loved Alice))))
ex15 = Sent (NP1 No Wizard) (VP1 Loved Dorothy)
ex16 = Sent (NP2 No (RCN1 Giant That 
                    (VP1 Defeated LittleMook))) 
            (VP1 Loved Dorothy)
ex17 = Sent (NP2 Some(RCN1 Princess That 
                     (VP1 Admired LittleMook))) 
            (VP1 Loved Dorothy)
ex19 = Sent (PRO 2) (VP1 Loved   (PRO 1))
ex20 = Sent (PRO 2) (VP1 Admired (PRO 1))
ex18 = Sent (NP1 The  Boy)  (VP1 Loved SnowWhite)
ex21 = Sent (NP1 Some Girl) (VP2 Admired Self)
ex22 = Sent (NP1 No   Boy)  (VP2 Admired Self)

nex1 = Sent He (VP1 Admired (NP1 Some Girl))

nex2 = Sent (NP1 Some Dwarf) (VP1 Defeated (NP1 The Giant))

nex2a = Sent (NP1 Some Dwarf) (VP1 Defeated (NP1 The Giant)) 
        `Txt` (Sent He Cheered)

nex2b = Sent (NP1 Some Dwarf) (VP1 Defeated (NP1 The Giant)) 
        `Txt` (Sent He (VP5 DidNot Cheer))

nex3 = (Sent LittleMook Cheered) `Txt` 
       (Sent He (VP1 Admired (NP1 Some Girl)))

nex4 = Txt (Sent (NP1 Some Dwarf) (VP5 DidNot Shudder))
           (Sent He (VP1 Defeated (NP1 Some Giant)))

nex5 = (Sent LittleMook (VP5 DidNot (INF1 Admire Dorothy)))
       `Txt` (Sent He Cheered)

nex6 = Txt (Sent (NP1 Some Dwarf) 
                 (VP5 DidNot (INF1 Admire Dorothy)))
           (Sent He (VP5 DidNot Cheer))

nex7 = Sent (NP1 Some Giant) 
            (VP5 DidNot (INF1 Admire (NP1 Some Princess)))

nex8  = (Sent (NP1 The Princess) (VP1 Defeated (NP1 The Giant))) 
        `Txt` (Sent She (VP1 Admired He))
nex9  = Sent He (VP1 Loved He)
nex10 = Sent He (VP2 Admired Self)
nex11 = Sent He (VP1 Admired He)
nex12 = Sent (NP1 The Giant ) (VP2 Admired Self)
nex13 = Txt (Sent (NP1 The Princess ) (VP2 Admired Self)) 
            (Sent She (VP1 Loved (NP1 The Giant)))
nex14 = Txt (Sent (NP1 Some Boy) (VP2 Admired Self))
            (Sent (NP1 Some Princess) (VP1 Loved He))
nex15 = If  (Sent (NP1 Some Boy) (VP2 Admired Self))
            (Sent (NP1 Some Giant) (VP1 Loved He))
nex16 = Txt (Sent (NP1 No Girl) (VP1 Helped LittleMook))
            (Sent (NP1 Some Princess) (VP1 Loved He))