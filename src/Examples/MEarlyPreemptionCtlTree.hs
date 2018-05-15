module Examples.MEarlyPreemptionCtlTree
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
data EndoId  = A | B | P_r | PS_r | NP_r | NPS_r | PNS_r | D deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "Early preemption (ctl-tree)"
         , cite = Footnote "Here the control flow is a tree." }

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        A -> f (Exo UA) -- shot
        B -> f (Exo UB) -- poison added
        P_r -> e B -- poisoned
        NP_r -> not (e B) -- not poisoned
        PS_r -> e P_r && e A -- poisoned but shot before it takes effect
        PNS_r -> e P_r && not (e A) -- poisoned but shot before it takes effect
        NPS_r -> e NP_r && e A -- not poisoned but still shot 
        D -> e PNS_r || e PS_r || e NPS_r -- dead if poisoned or shot earlier
        where f  = model u i
              e  = f . Endo

m u i (id) = model u i (Endo id)

u UA = True
u UB = True

phi = [(D,True)]
r= Set.fromList [A,B,D]
