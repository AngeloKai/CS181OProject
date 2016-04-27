import Data.List
import Data.Char
import Model
import P hiding (person)
import P_Type
import StringToEntityPredicate
import DRAC_Type
import SentEx
import SentConvert
import DRAC

parsedStr :: [ParseTree Cat Cat]
parsedStr = parses "The girl that laughed cheered"

result :: Bool
result = any ("COMP" ==) parsedStr