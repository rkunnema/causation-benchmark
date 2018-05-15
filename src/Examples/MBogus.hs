module Examples.MBogus
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
data EndoId  = P | A | S deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "Bogus prevention"
         , cite = Citation Nothing "causal-powers"}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        P -> f (Exo UP)
        A -> f (Exo UA)
        S ->  if e P then e A else True
        where f  = model u i
              e  = f . Endo

m u i (id) = model u i (Endo id)

u UP = False
u UA = True

phi = [(S,True)]
r= Set.fromList [P,A,S]
