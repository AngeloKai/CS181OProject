import EntityPredicate

data Sent = Sent NP VP | If Sent Sent | Txt Sent Sent
          deriving (Eq,Show)
data NP   = SnowWhite  | Alice | Dorothy | Goldilocks 
          | LittleMook | Atreyu
          | PRO Idx    | He | She | It
          | NP1 DET CN | NP2 DET RCN 
          deriving (Eq,Show)
data DET  = Every | Some | No | The 
          deriving (Eq,Show)
data CN   = Girl   | Boy    | Princess | Dwarf | Giant 
          | Wizard | Sword  | Poison 
          deriving (Eq,Show) 
data RCN  = RCN1 CN That VP | RCN2 CN That NP TV
          deriving (Eq,Show)
data That = That deriving (Eq,Show)
data REFL = Self deriving (Eq,Show)

data VP   = Laughed | Cheered | Shuddered 
          | VP1 TV NP    | VP2 TV REFL 
          | VP3 DV NP NP | VP4 DV REFL NP 
          | VP5 AUX INF  
          deriving (Eq,Show) 
data TV   = Loved   | Admired | Helped | Defeated
          deriving (Eq,Show)
data DV   = Gave deriving (Eq,Show)

data AUX  = DidNot deriving (Eq,Show) 

data INF  = Laugh | Cheer  | Shudder 
          | INF1  TINF NP  | INF2  DINF NP NP 
          deriving (Eq,Show) 
data TINF = Love  | Admire | Help | Defeat 
          deriving (Eq,Show) 
data DINF = Give deriving (Eq,Show) 

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
intNP (PRO i)    = \ p   -> p i 

intNP (NP1 det cn)  = (intDET det) (intCN cn) 
intNP (NP2 det rcn) = (intDET det) (intRCN rcn) 

intVP :: VP -> Idx -> Trans 
intVP Laughed          = blowupPred laugh
intVP Cheered          = blowupPred cheer
intVP Shuddered        = blowupPred shudder
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

intDV :: DV -> Idx -> Idx -> Idx -> Trans
intDV Gave = blowupPred3 give

intINF :: INF -> Idx -> Trans
intINF Laugh               = intVP Laughed
intINF Cheer               = intVP Cheered
intINF Shudder             = intVP Shuddered 
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

intRCN :: RCN -> Idx -> Trans
intRCN (RCN1 cn _ vp)   = \i -> conj (intCN cn i) 
                                     (intVP vp i)
intRCN (RCN2 cn _ np v) = \i -> conj (intCN cn i) 
                                     (intNP np (intTV v i))