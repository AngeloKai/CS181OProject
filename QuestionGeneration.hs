module QuestionGenerate where 

--import Data.List
--import Data.Char
--import Model
--import P hiding (person)
--import P_Type
--import StringToEntityPredicate
--import DRAC_Type
--import SentEx
--import SentConvert
--import DRAC 
import System.Random
import Control.Monad

genRandomInt :: (Num a, Random a) => IO a
genRandomInt = randomRIO (1, 4)

--sentToQ :: Sent -> Question 
--sentToQ (IF sent1 sent2) = 