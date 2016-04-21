-- Logical operator function 


-- Two different data types 
--type Context' = ([(Idx,Entity)],[Constraint])
--type Prop'    = [Context']
--type Trans'   = Context' -> Bool -> Prop'

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