module Examples.MHopkinsPearl
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

data ExoId  = UA | UB | UC  deriving Show
data EndoId  = A | B | C | D deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "Prisoner"
         , cite = Citation Nothing "hopkins2003clarifying"}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        A -> f (Exo UA)
        B -> f (Exo UB)
        C -> f (Exo UC)
        D -> ((e A) && (e B) || e C)
        where f = model u i
              e = (f . Endo)

m u i (id) = model u i (Endo id)

u UA = True
u UB = False
u UC = True

phi = [(D,True)]
r= Set.fromList [A,B,C,D]
