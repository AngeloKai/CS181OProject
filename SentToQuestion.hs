module SentToQuestion where

import Data.List
import Data.Char
import Model
import P hiding (person)
import P_Type
import StringToEntityPredicate
import DRAC_Type
import SentEx
import SentConvert





sentToQuestion :: Sent -> [String]

sentToQuestion Sent np "Laughed" = ["Who laughed?", np]
sentToQuestion Sent np "Cheered" = ["Who cheered?", np]
sentToQuestion Sent np "Shuddered" = ["Who shuddered?", np]

sentToQuestion Sent np (VP1 TV)

