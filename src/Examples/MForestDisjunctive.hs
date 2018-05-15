-- Model description: conjunctive forest fire
module Examples.MForestDisjunctive
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
import Examples.Name

data ExoId  = UB | US deriving Show
data EndoId  = B | S | O deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "--- disjunctive (overdet.)"
         , cite = Citation (Just "p. 278") "Hall2004-HALTCO-4"}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        B -> f (Exo UB)
        S -> f (Exo US)
        O -> (f $Endo B) || (f $Endo S)
        where f = model u i

m u i (id) = model u i (Endo id)


u UB = True
u US = True
phi = [(O,True)]
r= Set.fromList [B,S,O]
