module DRAC_Type where 

import Data.List
import Model
import P hiding (person)
import P_Type
import StringToEntityPredicate
import Utilities

type Context = [Entity]
type Prop    = [Context]
type Trans   = Context -> Prop
type Idx     = Int

type Context' = ([(Idx,Entity)],[Constraint])
type Prop'    = [Context']
type Trans'   = Context' -> Bool -> Prop'


data Sent = Sent NP VP | If Sent Sent | Txt Sent Sent
          deriving (Eq,Show,Read)

--instance Show Sent where 
--  show (Sent np vp) = (show np) ++ " " ++ (show vp)
--  show (If sent1 sent2) = (show sent1) ++ ", " ++ (show sent2)
--  show (Txt sent1 sent2) = (show sent1) ++ ". " ++ (show sent2) ++ "."   

data NP   = SnowWhite  | Alice | Dorothy | Goldilocks 
          | LittleMook | Atreyu | IsaacNewton | Him
          | PRO Idx    | He | She | It
          | NP1 DET CN | NP2 DET RCN 
          deriving (Eq,Show,Read)
--instance Show NP where
--    show (NP1 det cn) = (show det) ++ " " ++ (show cn) ++ " "
--    show (NP2 det rcn) = (show det) ++ " " ++ (show rcn) ++ " "
--    show IsaacNewton = "Isaac Newton"
--    show Him = "him"
--    show He = "he"

data RCN  = RCN1 CN That VP | RCN2 CN That NP TV
          deriving (Eq,Show, Read)
--instance Show RCN where
--  show (RCN1 cn that vp) = show cn ++ " that " ++ show vp ++ " "
--  show (RCN2 cn that np tv) = show cn ++ " that " ++ show np ++ " " ++ show tv ++ " "


data DET  = Every | Some | No | The | An
          deriving (Eq,Show,Read)


data CN   = Girl   | Boy    | Princess | Dwarf | Giant 
          | Wizard | Sword  | Poison | Telescope | Universe
          | Star | Person | Astronomer 
          deriving (Eq,Show,Read) 


data That = That deriving (Eq,Show,Read)
data REFL = Self deriving (Eq,Show,Read)

data VP   = Laughed | Cheered | Shuddered | Shined | Shines
          | VP1 TV NP    | VP2 TV REFL 
          | VP3 DV NP NP | VP4 DV REFL NP 
          | VP5 AUX INF  
          deriving (Eq,Show,Read) 
--instance Show VP where 
--  show (VP1 tv np) = show tv ++ " " ++ show np ++ " "
--  show (VP2 tv refl) = show tv ++ " " ++ show refl ++ " "
--  show (VP3 dv np1 np2) = show dv ++ " " ++ show np1 ++ " " ++ show np2 ++ " "
--  show (VP4 dv refl np) = show dv ++ " " ++ show refl ++ " " ++ show np ++ " "
--  show (VP5 aux inf) = show aux ++ " " ++ show inf ++ " "


data TV   = Loved   | Admired | Helped | Defeated | Owned
          | Studied | Is | Are | Was | Were
          deriving (Eq,Show,Read)
data DV   = Gave deriving (Eq,Show,Read)

data AUX  = DidNot deriving (Eq,Show,Read) 
  
--instance Show AUX where 
--  show (DidNot) = "did not "

data INF  = Laugh | Cheer  | Shudder | Shine 
          | INF1  TINF NP  | INF2  DINF NP NP 
          deriving (Eq,Show,Read) 

data TINF = Love  | Admire | Help | Defeat | Own
            | Study
          deriving (Eq, Show, Read)

data DINF = Give deriving (Eq,Show,Read) 


data Constraint = C1 VP Idx 
                | C2 TV Idx Idx 
                | C3 DV Idx Idx Idx
                | C4 VP Idx 
                | C5 TV Idx Idx 
                | C6 DV Idx Idx Idx
                deriving (Eq,Read)

instance Show Constraint where 
  show (C1 vp i)     = show vp ++ (' ':show i)
  show (C2 tv i j)   = show tv ++ (' ':show i) 
                               ++ (' ':show j)
  show (C3 dv i j k) = show dv ++ (' ':show i) 
                               ++ (' ':show j) 
                               ++ (' ':show k)

  show (C4 vp i)     = '-':show vp ++ (' ':show i)
  show (C5 tv i j)   = '-':show tv ++ (' ':show i) 
                                   ++ (' ':show j)
  show (C6 dv i j k) = '-':show dv ++ (' ':show i) 
                                   ++ (' ':show j) 
                                   ++ (' ':show k)
