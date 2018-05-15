module Examples.MForestDisjunctiveControlFlow
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

-- data Val = Bottom | Middle | Top | On | Off deriving (Show, Eq, Ord, Enum, Bounded)
data ExoId  = UMD | UL deriving Show
data EndoId  = MD | L | A | B | C | FF deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "Disj. CtlFlow"
         , cite = Footnote "TODO"}

-- Slightly different first light is matched, then 
-- Lighting hits, 
model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        MD -> f (Exo UMD)
        L -> f (Exo UL)
        Bogus -> False
        CBogus -> e MD && e L
        C -> e L && e MD
        FF -> 
        where f = model u i
              e = (f . Endo)

m u i (id) = model u i (Endo id)

u UMD = True
u UL  = True

phi = [(FF,True)]
r= Set.fromList [A,B,C,MD,L,FF]
