module Examples.MPushA
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
data EndoId  = P | B | T | H | D deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "Push A"
         , cite = Citation (Just "p.~26") "weslake2015partial"}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        P -> f (Exo UP)
        T -> f (Exo UT)
        B -> f (Exo UB)
        H -> e P && e T || e P && e B 
        D -> e H
        where f = model u i
              e = (f . Endo)

m u i (id) = model u i (Endo id)

u UP = True
u UT = True
u UB = True


phi = [(D,True)]
r= Set.fromList [P,T,B,H,D]
