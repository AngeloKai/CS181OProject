-- The file is used to interpret the following sentences
-- The girl laughed. No dwarf admired some princess that 
-- shuddered. Every girl that some boy loved cheered. The
-- wizard that helped Snow White defeated the giant. 


module DRAC where 

import Data.List
import Data.Char
import Model
import P hiding (person)
import P_Type
import StringToEntityPredicate
import DRAC_Type
import SentEx
import SentConvert



lookupIdx :: Context -> Idx -> Entity 
lookupIdx []     i = error "undefined context element"
lookupIdx (x:xs) 0 = x
lookupIdx (x:xs) i = lookupIdx xs (i-1)

extend :: Context -> Entity -> Context 
extend = \ c e -> c ++ [e]

neg :: Trans -> Trans
neg = \ phi c -> if phi c == [] then [c] else []

conj :: Trans -> Trans -> Trans 
conj = \ phi psi c -> concat [ psi c' | c' <- (phi c) ]

impl :: Trans -> Trans -> Trans 
impl = \ phi psi ->  neg (phi `conj` (neg psi))

exists :: Trans
exists = \ c -> [ (extend c x) | x <- [minBound..maxBound]]

forAll :: Trans -> Trans
forAll = \ phi -> neg (exists `conj` (neg phi))

context :: Context
context = [A,D,G,M,Y]

anchor :: Entity -> Context -> Idx 
anchor = \ e c -> anchor' e c 0 where 
  anchor' e []     i = error (show e ++ " is not anchored")
  anchor' e (x:xs) i | e == x    = i 
                     | otherwise = anchor' e xs (i+1)


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

dintTXT :: ParseTree Cat Cat -> Trans
dintTXT (Branch (Cat "_" "TXT" _ _) [s,cnj,txt])  = 
                (dintTXT s) `conj` (dintTXT txt)
dintTXT (Branch (Cat "_" "S" _ _)   [cond,s1,s2]) = 
                (dintTXT s1) `impl` (dintTXT s2)
dintTXT (Branch (Cat "_" "S" _ _)   [np,vp])      = 
                (dintNP np) (dintVP vp) 

dintREL :: ParseTree Cat Cat -> Idx -> Trans
dintREL (Branch (Cat  _  "COMP" _ _) [rel,s]) = dintREL s
dintREL (Branch (Cat  _  "COMP" _ _) [s])     = dintREL s 
dintREL (Branch (Cat "_" "S"    _ _) 
          [Leaf (Cat "#" "NP"   _ _),vp])     = dintVP vp 
dintREL (Branch (Cat "_" "S"    _ _) [np,vp]) = 
                       \ i -> (dintNP np) (dintVPgap vp i)

dintVPgap :: ParseTree Cat Cat -> Idx -> Idx -> Trans
dintVPgap (Branch (Cat  _   "VP" _  _) 
            [Leaf (Cat name "VP" _ [_]),
             Leaf (Cat "#"  "NP" _  _ )]) =
          blowupPred2 (name2binpred name)

dintNP :: ParseTree Cat Cat -> (Idx -> Trans) -> Trans
dintNP (Leaf (Cat name "NP" _ _)) = 
  \ p c -> p (anchor (name2entity name) c) c 
dintNP (Branch (Cat _ "NP" _ _) [det,cn]) = 
  (dintDET det) (dintCN cn) 

dintCN :: ParseTree Cat Cat -> Idx -> Trans
dintCN (Leaf (Cat name "CN" _ _)) = 
   blowupPred (name2pred name) 
dintCN (Branch (Cat _ "CN" _ _) [cn,rel]) = \ i -> 
  (dintCN cn i) `conj` (dintREL rel i)

dintDET :: ParseTree Cat Cat -> 
           (Idx -> Trans) -> (Idx -> Trans) -> Trans
dintDET (Leaf (Cat "every" "DET" _ _)) =
  \ phi psi c -> let i = length c in
        neg (exists `conj` (phi i) `conj` (neg (psi i))) c
dintDET (Leaf (Cat "some" "DET" _ _)) = 
  \ phi psi c -> let i = length c in
        (exists `conj` (phi i) `conj` (psi i)) c
dintDET (Leaf (Cat "a" "DET" _ _)) = 
  \ phi psi c -> let i = length c in
        (exists `conj` (phi i) `conj` (psi i)) c
dintDET (Leaf (Cat "no" "DET" _ _)) = 
  \ phi psi c -> let i = length c in
        neg (exists `conj` (phi i) `conj` (psi i)) c 
dintDET (Leaf (Cat "the" "DET" _ _)) = 
  \ phi psi c -> let i = length c in 
               ((unique (phi i)) `conj` 
                 exists `conj` (phi i) `conj` (psi i)) c

singleton :: [a] -> Bool
singleton [x] = True 
singleton  _  = False 

unique :: Trans -> Trans
unique phi c | singleton xs = [c]
             | otherwise    = []
    where xs = [ x | x <- entities, 
                     phi (extend c x) /= [] ]

dintVP :: ParseTree Cat Cat -> Idx -> Trans
dintVP (Branch (Cat _    "VP" _ _ ) 
         [Leaf (Cat name "VP" _ [])])          = 
        blowupPred (name2pred name) 
dintVP (Branch (Cat _    "VP" _  _) 
         [Leaf (Cat name "VP" _ [_]),np])      = 
        \ subj -> dintNP np (\ obj -> 
        (blowupPred2 (name2binpred name)) subj obj)
dintVP (Branch (Cat _    "VP" _  _   ) 
         [Leaf (Cat name "VP" _ [_,_]),np,pp]) = 
        \ subj -> dintNP np (\ iobj -> dintPP pp (\ dobj -> 
        (blowupPred3 (name2terpred name)) subj iobj dobj))

dintVP (Branch (Cat _        "VP" _ _ ) 
         [Leaf (Cat "did"    "AUX" _ []),vp]) = dintVP vp
dintVP (Branch (Cat _        "VP" _ _) 
         [Leaf (Cat "didn't" "AUX" _ []),vp]) = 
                                    \ i -> neg (dintVP vp i)

dintPP :: ParseTree Cat Cat -> (Idx -> Trans) -> Trans
dintPP (Branch (Cat _ "PP" _ _) [prep,np]) = dintNP np 

evl :: ParseTree Cat Cat -> Prop
evl = \ txt -> dintTXT txt context 

prc :: String -> [Prop]
prc string = map evl (parses string)

lift :: Trans -> Context -> (Context -> Bool) -> Bool
lift phi c p = any p (phi c)

intS :: Sent -> Trans
intS (Sent np vp) = (intNP np) (intVP vp)
intS (If   s1 s2) = (intS s1) `impl` (intS s2)
intS (Txt  s1 s2) = (intS s1) `conj` (intS s2)

intNP :: NP -> (Idx -> Trans) -> Trans
intNP SnowWhite  = \ p c -> p (anchor snowWhite  c) c
intNP Alice      = \ p c -> p (anchor alice      c) c
intNP Dorothy    = \ p c -> p (anchor dorothy    c) c
intNP Goldilocks = \ p c -> p (anchor goldilocks c) c
intNP LittleMook = \ p c -> p (anchor littleMook c) c
intNP IsaacNewton= \ p c -> p (anchor isaacNewton c) c
intNP (PRO i)    = \ p   -> p i 

intNP (NP1 det cn)  = (intDET det) (intCN cn) 
intNP (NP2 det rcn) = (intDET det) (intRCN rcn) 

intVP :: VP -> Idx -> Trans 
intVP Laughed          = blowupPred laugh
intVP Cheered          = blowupPred cheer
intVP Shuddered        = blowupPred shudder
intVP Shined           = blowupPred shine
intVP Shines           = blowupPred shines 
intVP (VP1 tv np)      = \s -> intNP np (\o -> intTV tv s o) 
intVP (VP2 tv _)       = self (intTV tv)
intVP (VP3 dv np1 np2) = \s -> intNP np1 (\io -> intNP np2 
                        (\o -> intDV dv s io o))
intVP (VP4 dv _ np)    = self (\s io -> intNP np (\o -> 
                                        intDV dv s io o))
intVP (VP5 _not inf)   = \s -> neg (intINF inf s)

intTV :: TV -> Idx -> Idx -> Trans
intTV Loved    = blowupPred2 love 
intTV Admired  = blowupPred2 admire 
intTV Helped   = blowupPred2 help
intTV Defeated = blowupPred2 defeat
intTV Owned    = blowupPred2 own
intTV Studied  = blowupPred2 study

intDV :: DV -> Idx -> Idx -> Idx -> Trans
intDV Gave = blowupPred3 give

intINF :: INF -> Idx -> Trans
intINF Laugh               = intVP Laughed
intINF Cheer               = intVP Cheered
intINF Shudder             = intVP Shuddered
intINF Shine               = intVP Shined 
intINF (INF1 tinf np)      = \s -> intNP np (\o -> 
                                   intTINF tinf s o)
intINF (INF2 dinf np1 np2) = \s -> intNP np1 (\io -> 
                                   intNP np2 (\o  -> 
                                   intDINF dinf s io o))

intTINF :: TINF -> Idx -> Idx -> Trans
intTINF Love   = intTV Loved
intTINF Admire = intTV Admired
intTINF Help   = intTV Helped
intTINF Defeat = intTV Defeated
intTINF Own    = intTV Owned
intTINF Study  = intTV Studied

intDINF :: DINF -> Idx -> Idx -> Idx -> Trans
intDINF Give   = intDV Gave

intCN :: CN -> Idx -> Trans
intCN Girl     = blowupPred girl
intCN Boy      = blowupPred boy
intCN Princess = blowupPred princess
intCN Dwarf    = blowupPred dwarf
intCN Giant    = blowupPred giant
intCN Wizard   = blowupPred wizard
intCN Sword    = blowupPred sword
intCN Telescope= blowupPred telescope
intCN Universe = blowupPred universe
intCN Star     = blowupPred star
intCN Person   = blowupPred person



intDET :: DET -> (Idx -> Trans) -> (Idx -> Trans) -> Trans

intDET Some  = \ phi psi c -> let i = length c in
               (exists `conj` (phi i) `conj` (psi i)) c 
intDET Every = \ phi psi c -> let i = length c in
       neg (exists `conj` (phi i) `conj` (neg (psi i))) c

intDET No    = \ phi psi c -> let i = length c in
             neg (exists `conj` (phi i) `conj` (psi i)) c 
intDET The   = \ phi psi c -> let i = length c in 
               ((unique (phi i)) `conj` 
                 exists `conj` (phi i) `conj` (psi i)) c
intDET An     = \ phi psi c -> let i = length c in
               (exists `conj` (phi i) `conj` (psi i)) c                

intRCN :: RCN -> Idx -> Trans
intRCN (RCN1 cn _ vp)   = \i -> conj (intCN cn i) 
                                     (intVP vp i)
intRCN (RCN2 cn _ np v) = \i -> conj (intCN cn i) 
                                     (intNP np (intTV v i))

eval :: Sent -> Prop
eval = \ s -> intS s context 


maxIndex  :: Constraint -> Idx
maxIndex (C1 vp i)     = i
maxIndex (C2 tv i j)   = max i j 
maxIndex (C3 dv i j k) = maximum [i,j,k]
maxIndex (C4 vp i)     = i
maxIndex (C5 tv i j)   = max i j 
maxIndex (C6 dv i j k) = maximum [i,j,k]


size :: Context' -> Int
size (c,co) = length c

lookupIdx' :: Context' -> Idx -> Entity 
lookupIdx' ([],co)       j = error "undefined context item"
lookupIdx' ((i,x):xs,co) j | i == j    = x
                           | otherwise = lookupIdx' (xs,co) j

adjust :: (Idx,Entity) -> Context' -> Context'
adjust (i,x) (c,co) 
     | elem (i,x) c = (((i,x):(filter (/=(i,x)) c)),co)
     | otherwise    = error "item not found in context"

extend' :: Context' -> Entity -> Context' 
extend' = \ (c,co) e -> let i = length c in (((i,e):c),co)

success :: Context' -> Trans' -> Bool
success = \ c phi -> phi c True /= []

cutoff :: [Context'] -> Idx -> [Context']
cutoff []          i = []
cutoff ((c,co):cs) i = (cutoffc c i,cutoffco co i)
                      :(cutoff cs i) 
  where 
     cutoffc []         i             = []
     cutoffc ((j,x):xs) i | j >= i    = cutoffc xs i
                          | otherwise = (j,x):(cutoffc xs i)
     cutoffco []        i             = []
     cutoffco (co:cos)  i 
                   | maxIndex co >= i = cutoffco cos i
                   | otherwise        = co:(cutoffco cos i)

neg' :: Trans' -> Trans'
neg' = \ phi c b -> if b then phi c False
                         else cutoff (phi c True) (size c)

conj' :: Trans' -> Trans' -> Trans' 
conj' = \ phi psi c b -> if b 
      then concat [ psi c' True | c' <- phi c True ] 
      else if any (\c' -> psi c' True /= []) (phi c True)
           then []
           else if   (phi c True) == [] then (phi c False)
                else nub (cutoff (concat [psi c' False  | 
                                          c' <- phi c True]) 
                                 (size c))

impl' ::  Trans' -> Trans' -> Trans' 
impl' = \ phi psi ->  neg' (phi `conj'` (neg' psi))

exists' :: Trans'
exists' = \ c b -> if   b 
                   then [ (extend' c e) | e <- entities ]
                   else []

blowupPred' :: (Entity -> Bool) -> Idx -> Trans'
blowupPred' = \ pred i c  b -> 
     let 
         e  = lookupIdx' c i 
         c' = adjust (i,e) c
     in  
         if  b 
         then if   pred e 
              then [c'] 
              else []
         else if   pred e 
              then [] 
              else [c']

blowupVP :: VP -> OnePlacePred -> Idx -> Trans'
blowupVP = \ vp pred i c b -> 
         let 
             e        = lookupIdx' c i 
             (c',cos) = adjust (i,e) c
             co       = C1 vp i
             co'      = C4 vp i
         in  
             if   b 
             then if   pred e 
                  then [(c',co:cos)] 
                  else []
             else if   pred e 
                  then [] 
                  else [(c',co':cos)]

blowupTV :: TV -> TwoPlacePred -> Idx -> Idx -> Trans'
blowupTV = \ tv pred subj obj c b -> 
        let 
            e1       = lookupIdx' c subj
            e2       = lookupIdx' c obj 
            (c',cos) = adjust (subj,e1) (adjust (obj,e2) c)
            co       = C2 tv subj obj
            co'      = C5 tv subj obj
        in  
            if   b 
            then if   pred e1 e2 
                 then [(c',co:cos)] 
                 else []
            else if pred e1 e2 
                 then [] 
                 else [(c',co':cos)]

blowupDV :: DV  -> ThreePlacePred -> 
            Idx -> Idx -> Idx -> Trans'
blowupDV = \ dv pred subj iobj dobj c b -> 
        let 
            e1       = lookupIdx' c subj
            e2       = lookupIdx' c iobj 
            e3       = lookupIdx' c dobj 
            (c',cos) = adjust (subj,e1) 
                      (adjust (iobj,e2)
                      (adjust (dobj,e3) c))
            co       = C3 dv subj iobj dobj
            co'      = C6 dv subj iobj dobj
        in  
            if   b 
            then if   pred e1 e2 e3 
                 then [(c',co:cos)] 
                 else []
            else if   pred e1 e2 e3 
                 then [] 
                 else [(c',co':cos)]

resolveMASC :: Context' -> [Idx]
resolveMASC (c,co)  = resolveMASC' c where
  resolveMASC' []                     = [] 
  resolveMASC' ((i,x):xs) | male x    = i : resolveMASC' xs
                          | otherwise = resolveMASC' xs

resolveFEM :: Context' -> [Idx]
resolveFEM (c,co)  = resolveFEM' c where
  resolveFEM' []                     = [] 
  resolveFEM' ((i,x):xs) | female x  = i : resolveFEM' xs
                         | otherwise = resolveFEM' xs

resolveNEU :: Context' -> [Idx]
resolveNEU (c,co)  = resolveNEU' c where
  resolveNEU'  []                     = [] 
  resolveNEU'  ((i,x):xs) | thing x   = i : resolveNEU' xs
                          | otherwise = resolveNEU' xs

resolveNAME :: Entity -> Context' -> (Idx,Context')
resolveNAME x c | i /= -1   = (i,c)
                | otherwise = (j,extend' c x)
  where i                                 = index x c 
        j                                 = size c 
        index x ([],co)                   = -1
        index x ((i,y):xs,co) | x == y    = i 
                              | otherwise = index x (xs,co)

nonCoref :: (Idx -> Idx -> Trans') -> Idx -> Idx -> Trans'
nonCoref = \ p i j c b -> if   i /= j 
                          then (p i j c b) 
                          else []

nonCoref2 :: (Idx -> Idx -> Idx -> Trans') ->
              Idx -> Idx -> Idx -> Trans'
nonCoref2 = \ p i j k c b -> if   i /= j && j /= k && i /= k 
                             then (p i j k c b) 
                             else []

intS' :: Sent -> Trans'
intS' (Sent np vp) = (intNP' np) (intVP' vp)
intS' (If   s1 s2) = (intS' s1) `impl'` (intS' s2)
intS' (Txt  s1 s2) = (intS' s1) `conj'` (intS' s2)

intNP' :: NP -> (Idx -> Trans') -> Trans'
intNP' SnowWhite  = \p c -> 
                    let (i,c') = resolveNAME snowWhite c
                    in  p i c'
intNP' Alice      = \p c -> 
                    let (i,c') = resolveNAME alice c
                    in  p i c'
intNP' Dorothy    = \p c -> 
                    let (i,c') = resolveNAME dorothy c
                    in  p i c'
intNP' Goldilocks = \p c -> 
                    let (i,c') = resolveNAME goldilocks c
                    in  p i c'
intNP' LittleMook = \p c -> 
                    let (i,c') = resolveNAME littleMook c
                    in  p i c'
intNP' IsaacNewton= \p c -> 
                    let (i,c') = resolveNAME isaacNewton c
                    in  p i c'                    

intNP' He  = \p c b -> concat [p i c b | i <- resolveMASC c]
intNP' She = \p c b -> concat [p i c b | i <- resolveFEM  c]
intNP' It  = \p c b -> concat [p i c b | i <- resolveNEU  c]
intNP' Him = \p c b -> concat [p i c b | i <- resolveMASC c]
intNP' (PRO i)       = \ p c ->  p i c 
intNP' (NP1 det cn)  = (intDET' det) (intCN' cn) 
intNP' (NP2 det rcn) = (intDET' det) (intRCN' rcn)

intVP' :: VP -> Idx -> Trans'
intVP' (VP1 tv np)      = \ s -> intNP'  np (\ o -> 
                          nonCoref (intTV' tv) s o) 
intVP' (VP2 tv refl)    = self (intTV' tv)
intVP' (VP3 dv np1 np2) = \ s -> intNP' np1 (\ io -> 
                                 intNP' np2 (\ o  -> 
                          nonCoref2 (intDV' dv) s io o))
intVP' (VP4 dv refl np) = self (\ s io -> intNP' np (\ o -> 
                                          intDV' dv s io o))
intVP' (VP5 _not inf)   = \ s -> neg' (intINF' inf s)

intVP' Laughed   = blowupVP Laughed   laugh
intVP' Cheered   = blowupVP Cheered   cheer 
intVP' Shuddered = blowupVP Shuddered shudder
intVP' Shined    = blowupVP Shined shine

intTV' :: TV -> Idx -> Idx -> Trans'
intTV' Loved    = blowupTV Loved    love 
intTV' Admired  = blowupTV Admired  admire 
intTV' Helped   = blowupTV Helped   help 
intTV' Defeated = blowupTV Defeated defeat
intTV' Owned    = blowupTV Owned own
intTV' Studied  = blowupTV Studied study
intTV' Was      = blowupTV Was was


intDV' :: DV -> Idx -> Idx -> Idx -> Trans'
intDV' Gave     = blowupDV Gave     give

intINF' :: INF -> Idx -> Trans'
intINF' Laugh               = intVP' Laughed
intINF' Cheer               = intVP' Cheered
intINF' Shudder             = intVP' Shuddered
intINF' Shine               = intVP' Shined
intINF' (INF1 tinf np)      = \ s -> intNP' np  (\ o -> 
                                     intTINF' tinf s o)
intINF' (INF2 dinf np1 np2) = \ s -> intNP' np1 (\ io -> 
                                     intNP' np2 (\ o  -> 
                                     intDINF' dinf s io o))


intTINF' :: TINF -> Idx -> Idx -> Trans'
intTINF' Love   = intTV' Loved
intTINF' Admire = intTV' Admired
intTINF' Help   = intTV' Helped
intTINF' Defeat = intTV' Defeated
intTINF' Own    = intTV' Owned
intTINF' Study  = intTV' Studied


intDINF' :: DINF -> Idx -> Idx -> Idx -> Trans'
intDINF' Give   = intDV' Gave

intCN' :: CN -> Idx -> Trans'
intCN' Girl     = blowupPred' girl 
intCN' Boy      = blowupPred' boy
intCN' Princess = blowupPred' princess
intCN' Dwarf    = blowupPred' dwarf
intCN' Giant    = blowupPred' giant
intCN' Wizard   = blowupPred' wizard
intCN' Sword    = blowupPred' sword
intCN' Telescope= blowupPred' telescope
intCN' Universe = blowupPred' universe
intCN' Star     = blowupPred' star
intCN' Person   = blowupPred' person
intCN' Astronomer = blowupPred' astronomer


unique' :: Idx -> Trans' -> Trans'
unique' i phi c b = 
 if b == singleton xs then [c] else [] 
   where xs = [ x | x <- entities, success (extend' c x) phi ]

intDET' :: DET -> (Idx -> Trans') 
               -> (Idx -> Trans') -> Trans'
intDET' Some  = \ phi psi c -> let i = size c in 
                 (exists' `conj'` (phi i) `conj'` (psi i)) c
intDET' Every = \ phi psi c -> let i = size c in 
                (impl' (exists' `conj'` (phi i)) 
                       (psi i)) c
intDET' No    = \ phi psi c -> let i = size c in 
                (impl' (exists' `conj'` (phi i)) 
                       (neg' (psi i))) c
intDET' The   = \ phi psi c -> let i = size c in 
                (conj' (unique' i (phi i)) 
                        exists' `conj'` (phi i) 
                                `conj'` (psi i)) c
intDET' An     = \ phi psi c -> let i = size c in 
                 (exists' `conj'` (phi i) `conj'` (psi i)) c

intRCN' :: RCN -> Idx -> Trans'
intRCN' (RCN1 cn _ vp)    = \i -> conj' (intCN' cn i) 
                                        (intVP' vp i)
intRCN' (RCN2 cn _ np tv) = \i -> conj' (intCN' cn i) 
                             (intNP' np (intTV' tv i))

convert :: Context -> Context'
convert c = (convert' c (length c - 1),[]) 
       where convert' []     i = []
             convert' (x:xs) i = (i,x):(convert' xs (i-1))

eval' :: Sent -> Prop'
eval' s = intS' s (convert context) True

evalFresh :: Sent -> Prop'
evalFresh s = intS' s ([],[]) True


