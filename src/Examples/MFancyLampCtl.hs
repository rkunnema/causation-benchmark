module Examples.MFancyLampCtl
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
data EndoId  = A | B | C | N1 | N2 | N3 | L deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "--- (ctl)"
         , cite = Citation (Just "p.~31") "weslake2015partial"}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        A  -> f (Exo UA)
        B  -> f (Exo UB)
        C  -> f (Exo UC)
        N1 -> b (e A == Bottom || e B == Bottom || e A == Bottom)
        N2 -> b (e A == Middle || e B == Middle || e A == Middle)
        N3 -> b (e A == Top || e B == Top || e A == Top)
        L  -> b (e N1 == On || e N2 == On || e N3 == On)
        where f = model u i
              e = (f . Endo)
              b True = On
              b False = Off

m u i (id) = model u i (Endo id)

u UA = Top
u UB = Bottom
u UC = Bottom

phi = [(L,On)]
r= Set.fromList [A,B,C,L]
