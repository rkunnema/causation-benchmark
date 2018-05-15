module Examples.MPushB
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

data Val = Off | On | ToSafety deriving (Show, Eq, Ord, Enum, Bounded)
data ExoId  = UP | UT | UB deriving Show
data EndoId  = P | T | B | H | D deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "Push B"
         , cite = Citation (Just "p.~26") "weslake2015partial"}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        P -> f (Exo UP)
        T -> f (Exo UT)
        B -> f (Exo UB)
        H -> if (e P == On && e T == On) || (e P == Off && e B == On) then On else Off
        D -> e H
        where f = model u i
              e = (f . Endo)

m u i (id) = model u i (Endo id)

u UP = On
u UT = On
u UB = On

phi = [(D,On)]
r= Set.fromList [P,T,B,H,D]
