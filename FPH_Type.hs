
module FPH_Type 

where

import Data.List
import Data.Char

type Sentences = [Sentence]

type Phoneme = [Feature]

data DeclClass = One | Two | Three | Four | Five 

data Subject   = Chomsky | Montague deriving Show
data Predicate = Wrote String       deriving Show

data Sentence  = S Subject Predicate 

instance Show Sentence where
  show (S subj pred) = show subj ++ " " ++ show pred

data Colour = RGB Int Int Int deriving Show

data Feature = F Attr Value deriving (Eq,Show)

data Attr    = Back | High | Round | Cons deriving (Eq,Show)
data Value   = Plus | Minus               deriving (Eq,Show)
