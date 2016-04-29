module DRAC_Type where 

import Data.List
import Model
import P hiding (person)
import P_Type
import StringToEntityPredicate

type Context = [Entity]
type Prop    = [Context]
type Trans   = Context -> Prop
type Idx     = Int

type Context' = ([(Idx,Entity)],[Constraint])
type Prop'    = [Context']
type Trans'   = Context' -> Bool -> Prop'

data Sent = Sent NP VP | If Sent Sent | Txt Sent Sent
          deriving (Eq,Show,Read)

data NP   = SnowWhite  | Alice | Dorothy | Goldilocks 
          | LittleMook | Atreyu 
          | PRO Idx    | He | She | It
          | NP1 DET CN | NP2 DET RCN 
          deriving (Eq,Show,Read)

data RCN  = RCN1 CN That VP | RCN2 CN That NP TV
          deriving (Eq,Show,Read)

data DET  = Every | Some | No | The 
          deriving (Eq,Show,Read)
data CN   = Girl   | Boy    | Princess | Dwarf | Giant 
          | Wizard | Sword  | Poison 
          deriving (Eq,Show,Read) 

data That = That deriving (Eq,Show,Read)
data REFL = Self deriving (Eq,Show,Read)

data VP   = Laughed | Cheered | Shuddered
          | VP1 TV NP    | VP2 TV REFL 
          | VP3 DV NP NP | VP4 DV REFL NP 
          | VP5 AUX INF  
          deriving (Eq,Show,Read) 
data TV   = Loved   | Admired | Helped | Defeated
          deriving (Eq,Show,Read)
data DV   = Gave deriving (Eq,Show,Read)

data AUX  = DidNot deriving (Eq,Show,Read) 

data INF  = Laugh | Cheer  | Shudder 
          | INF1  TINF NP  | INF2  DINF NP NP 
          deriving (Eq,Show,Read) 
data TINF = Love  | Admire | Help | Defeat 
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
