module Examples.MBogusHalpernReversed
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

data ExoId  = UP | UA  deriving Show
data EndoId  = P | A | D | NP_r | P_r | PN_r deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "--- (ctl, reversed)"
         , cite = Footnote "Like Ex. \\ref{ex:bogus}, but reversed control flow: $D=0$ iff $\\mathit{NP}_r=\\top$ (poison not administered) or $\\mathit{PN}_r=\\top$ (poison neutralised)."}

model u i (Exo id) = u id
model u i (Endo id) = case i id of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        P -> f (Exo UP)
        A -> f (Exo UA)
        NP_r -> not(e P)
        P_r -> e P
        PN_r -> e P_r && e A    -- poison was neutralized 
        D -> not ( e NP_r || e PN_r) -- dead if unless not poisoned or neutralized
        where f  = model u i
              e = (f . Endo)

m u i id = model u i (Endo id)

u UP = False
u UA = True

phi = [(D,False)]
r= Set.fromList [P,A,D]
