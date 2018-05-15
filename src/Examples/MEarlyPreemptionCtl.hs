module Examples.MEarlyPreemptionCtl
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
data EndoId  = A | B | P_r | S_r | PE_r | NS_r | D deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "--- (ctl), Ex.~\\ref{ex:shot-poisoned}"
         , cite = Footnote "Model with $\\Vrch=\\set{P_r,S_r,\\mathit{PE}_r, \\mathit{NS}_r}$ and $P_r=B$, $S_r=A$, $\\mathit{NS}_r=\\neg A$, $\\mathit{PE}_r=P_r \\land \\neg A$ and $D=1$ iff $S_r=\\top$ or $\\mathit{PE}_r=\\top$." }
         -- , cite = Example "ex:shot-poisoned" 

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        A -> f (Exo UA) -- shot
        B -> f (Exo UB) -- poison added
        P_r -> e B -- poison added
        S_r -> e A -- shot after poison added or not
        NS_r -> not (e A) -- not shot 
        PE_r -> (e P_r) && (e NS_r) -- poison takes effect because not shot
        D -> e S_r || e PE_r -- dead if poison had effect; or shot earlier
        where f  = model u i
              e  = f . Endo

m u i (id) = model u i (Endo id)

u UA = True
u UB = True

phi = [(D,True)]
r= Set.fromList [A,B,D]
