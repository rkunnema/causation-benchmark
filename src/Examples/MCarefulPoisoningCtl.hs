module Examples.MCarefulPoisoningCtl
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

data ExoId  = UA deriving Show
data EndoId  = A | NA_r | A_r | P_r | P | D  deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "--- (ctl)"
         , cite = Footnote "Model with $\\Vrch=\\set{\\mathit{NA}_r,A_r,P_r}$ and $\\mathit{NA}_r=\\top$ iff $A=0$, $A_r=\\top$ iff $A=1$, $P_r=\\top$ iff $NA_r=\\top\\land P=1$ and $D=1$ iff $P_r=\\top$." }

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        A -> f (Exo UA)
        NA_r -> not(e A)
        A_r -> e A
        P -> e A_r
        P_r -> e NA_r && e P
        D -> e P_r -- dead if unless not poisoned or neutralized
        where f  = model u i
              e  = (f . Endo)

m u i id = model u i (Endo id)

u UA = True

phi = [(D,False)]
r= Set.fromList [A,P,D]
