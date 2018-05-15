module Examples.MCarefulPoisoning
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
data EndoId  = A |  P | D  deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "Careful Poisoning"
         , cite = Citation (Just "Ex.~11") "weslake2015partial"}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        A -> f (Exo UA)
        P -> e A
        D -> not (e A) && e P
        where f  = model u i
              e  = (f . Endo)

m u i id = model u i (Endo id)

u UA = True

phi = [(D,False)]
r= Set.fromList [A,P,D]
