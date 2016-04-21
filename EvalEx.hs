eval :: Sent -> Prop
eval = \ s -> intS s context 

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
