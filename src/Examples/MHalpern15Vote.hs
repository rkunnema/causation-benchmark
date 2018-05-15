module Examples.MHalpern15Vote
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

data Value = ZERO | ONE | TWO deriving (Show,Eq,Ord,Enum,Bounded)
-- no control flow variable
data ExoId  = UV_1 | UV_2  deriving Show
data EndoId  = V_1 | V_2 | M | P deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "Vote"
         , cite = Citation (Just "Ex.~4.1") "DBLP:conf/ijcai/Halpern15"}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        V_1 -> f (Exo UV_1)
        V_2 -> f (Exo UV_2)
        M -> if e V_1 /= e V_2 then ONE
             else if e V_1 == ONE then TWO
             else ZERO
        P -> if e M == ZERO then ZERO
             else ONE
        where f  = model u i
              e  = f . Endo

m u i (id) = model u i (Endo id)

u UV_1 = ONE
u UV_2 = ONE

phi = [(P,ONE)]
r= Set.fromList [V_1,V_2,M,P]
