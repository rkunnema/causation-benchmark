module Examples.MBogusCtl
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
data EndoId  = P | A | S | P_r | NP_r | N_r deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "--- (ctl)"
         , cite =  Example "ex:bogus"}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        P -> f (Exo UP)
        A -> f (Exo UA)
        NP_r -> not $e P
        P_r -> e P
        N_r -> e P_r && e A
        S ->  if e NP_r || e N_r then True else False
        where f  = model u i
              e  = (f . Endo)

m u i (id) = model u i (Endo id)

u UP = False
u UA = True

phi = [(S,True)]
r= Set.fromList [P,A,S]
