module Examples.MCombinationLampHalpern
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

data Val = Bottom | Middle | Top | On | Off deriving (Show, Eq, Ord, Enum, Bounded)
data ExoId  = UA | UB | UC  deriving Show
data EndoId  = A | B | C | L deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "---" -- "Combination Lamp"
         , cite = Citation (Just "Ex.~3.2") "halpern2016appropriate"}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        A -> f (Exo UA)
        B -> f (Exo UB)
        C -> f (Exo UC)
        L -> if (e A == e B || e B == e C || e A == e C)
             then On else Off
        where f = model u i
              e = (f . Endo)

m u i (id) = model u i (Endo id)

u UA = Top
u UB = Bottom
u UC = Bottom

phi = [(L,On)]
r= Set.fromList [A,B,C,L]
