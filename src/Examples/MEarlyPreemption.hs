module Examples.MEarlyPreemption
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

data ExoId  = UA | UB  deriving Show
data EndoId  = A | B | D_1 | P | D_2 deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "Early preemption"
         , cite = Citation (Just "p. 526") "Hitchcock2007-HITPPA"}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        A -> f (Exo UA) -- shot
        B -> f (Exo UB) -- poison added
        D_1 -> e A -- shot
        P -> (e B) && not (e D_1) -- poison takes effect only if not shot earlier
        D_2 -> (e P) || (e D_1) -- dead if poisoned or shot earlier
        where f  = model u i
              e  = f . Endo

m u i (id) = model u i (Endo id)

u UA = True
u UB = True

phi = [(D_2,True)]
r= Set.fromList [A,B,D_2]
