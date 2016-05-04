-- Data types in P 

module P_Type where 

-- Chapter 9 parsed trees
import Prelude hiding ((<*>),(<$>))
import Data.List
import Data.Char
import FPH

-- type of parser taking a list of items of type a and building
-- output of type b
type Parser a b = [a] -> [(b,[a])]

-- Parser that takes a list of a's and returns a pair consisting of
-- a parse tree and a list of a's not yet used.
type PARSER a b = Parser a (ParseTree a b)

type StackParser a b = [a] -> [a] -> [(b,[a],[a])]

type SPARSER a b = StackParser a (ParseTree a b)

type Agreement = [Feat]
type CatLabel = String
type Phon     = String
-- A position in the parse tree is a list of indices giving directions from the root
type Pos = [Int]
-- Type of a binary relation on a
type Rel a = [(a,a)]
type Words = [String]

-- A parse tree for English
data ParseTree a b =  Ep | Leaf a | Branch b [ParseTree a b] 
                   deriving (Eq, Read)
-- TODO: change the show instance part back 
-- instance (Show a, Show b) => Show (ParseTree a b) where
--   show Ep            = "[]"
--   show (Leaf t)      = show t
--   show (Branch l ts) = "[." ++ show l  ++ " " 
--                             ++ show ts ++ "]"
instance (Show a, Show b) => Show (ParseTree a b) where
  show Ep            = "Ep"
  show (Leaf t)      = "Leaf " ++ show t
  show (Branch l ts) = "Branch " ++ show l  ++ " " 
                                ++ show ts


-- Section 9.4, Features and Categories
data Feat = Masc  | Fem  | Neutr | MascOrFem 
          | Sg    | Pl 
          | Fst   | Snd  | Thrd 
          | Nom   | AccOrDat 
          | Pers  | Refl | Wh 
          | Tense | Infl
          | On    | With | By | To | From
          | Hum   | Obj  
          deriving (Eq, Show, Read, Ord)


data Cat      = Cat Phon CatLabel Agreement [Cat]
              deriving (Eq, Read)

-- instance Show Cat where
--   show (Cat "_"  label agr subcatlist) = label ++ show agr
--   show (Cat phon label agr subcatlist) = phon  ++ " " 
--                                                ++ label ++ show agr
instance Show Cat where
  show (Cat "_"  label agr subcatlist) = "(Cat " ++ label ++ show agr ++ " )"
  show (Cat phon label agr subcatlist) = "(Cat " ++ phon  ++ " " ++ label 
                                                 ++ " " ++ show agr ++ " )"


data Term = Const String | Var Int deriving (Eq,Ord)

data GQ = Sm | All | Th | Most | Many | Few 
        deriving (Eq,Show,Ord) 

data Abstract = MkAbstract Int LF deriving (Eq,Ord) 

data LF = Rel String [Term] 
        | Eq   Term Term
        | Neg  LF 
        | Impl LF LF 
        | Equi LF LF 
        | Conj [LF]
        | Disj [LF] 
        | Qt GQ Abstract Abstract 
     deriving (Eq,Ord)

instance Show Term where
  show (Const name) = name 
  show (Var i)      = 'x': show i

instance Show Abstract where 
  show (MkAbstract i lf) = 
   "(\\ x" ++ show i ++ " " ++ show lf ++ ")"

instance Show LF where
  show (Rel r args)   = r ++ show args
  show (Eq t1 t2)     = show t1 ++ "==" ++ show t2
  show (Neg lf)       = '~': (show lf)
  show (Impl lf1 lf2) = "(" ++ show lf1 ++ "==>" 
                            ++ show lf2 ++ ")"
  show (Equi lf1 lf2) = "(" ++ show lf1 ++ "<=>" 
                            ++ show lf2 ++ ")"
  show (Conj [])      = "true" 
  show (Conj lfs)     = "conj" ++ concat [ show lfs ]
  show (Disj [])      = "false" 
  show (Disj lfs)     = "disj" ++ concat [ show lfs ]
  show (Qt gq a1 a2)   = show gq ++ (' ' : show a1) 
                                 ++ (' ' : show a2)