module Examples.MShock
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
data EndoId  = A | B | C | C1 | C2 deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "Shock"
         , cite = Citation (Just "p.~17") "weslake2015partial"}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        A -> f (Exo UA)
        C1 -> e A
        C2 -> not (e A)
        B -> if e C1 then True else False
        C -> e A == e B
        where f = model u i
              e = (f . Endo)

m u i (id) = model u i (Endo id)

u UA = True

phi = [(C,True)]
r= Set.fromList [A,B,C]
