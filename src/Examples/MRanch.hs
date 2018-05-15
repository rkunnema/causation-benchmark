module Examples.MRanch
(
      n
    , m
    , u
    , phi
    , r
)
where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import Examples.Name

data Value = OFF | ZERO | ONE  deriving (Show,Eq,Ord,Enum,Bounded)
-- minBound::Value should give Off value for control flow variable
data ExoId  = UA_1 | UA_2 | UA_3 | UA_4 | UA_5  deriving Show
data EndoId  = A_1 | A_2 | A_3 | A_4 | A_5 | M_1 | M_2 | M_3 | O deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "Ranch"
         , cite = Citation (Just "Ex.~3.7") "DBLP:conf/ijcai/Halpern15"}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        A_1 -> f (Exo UA_1)
        A_2 -> f (Exo UA_2)
        A_3 -> f (Exo UA_3)
        A_4 -> f (Exo UA_4)
        A_5 -> f (Exo UA_5)
        M_1 -> if e A_1 == e A_2 then e A_1 else OFF  
        M_2 -> if e A_1==ZERO && List.all ((==ONE) . e) [A_2,A_3,A_4,A_5] then ZERO
               else if (e A_1==ONE) && List.all ((==ZERO) . e) [A_2,A_3,A_4,A_5] then ONE
               else OFF
        M_3 -> if length ( List.filter ((==ONE) . e) [A_1,A_2,A_3,A_4,A_5]) > 2 then ONE
               else ZERO
        O -> if ((e M_1) /= OFF) || ((e M_2) /= OFF) then (e A_1) 
             else e M_3
        where f  = model u i
              e  = f . Endo

m u i (id) = model u i (Endo id)

u UA_1 = ONE
u UA_2 = ONE
u UA_3 = ZERO
u UA_4 = ZERO
u UA_5 = ZERO

phi = [(O,ONE)]
r= Set.fromList [A_1,A_2,A_3,A_4,A_5]
