module Examples.MPushCtl
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

data ExoId  = UP | UB | UT deriving Show
data EndoId  = P | B | T | P_r | NP_r | H | D deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "--- (ctl)"
         , cite = Footnote "Model with $\\Vrch=\\set{P_r,\\mathit{NP}_r}$, where $P_r=\\top$ iff $P=1$, $\\mathit{NP}_r=\\top$ iff $P=0$ and $H=1$ iff either $P_r=\\top \\land T=1$  or $\\mathit{NP}_r=\\top \\land B=1$."}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        P -> f (Exo UP)
        T -> f (Exo UT)
        B -> f (Exo UB)
        P_r -> e P
        NP_r -> not (e P)
        H -> if e P_r then e T else if e NP_r then e P else False
        D -> e H
        where f = model u i
              e = (f . Endo)

m u i (id) = model u i (Endo id)

u UP = True
u UT = True
u UB = True

phi = [(D,True)]
r= Set.fromList [P,B,T,H,D]
