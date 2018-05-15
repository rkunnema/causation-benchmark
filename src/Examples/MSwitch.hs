module Examples.MSwitch
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

data ExoId  = US deriving Show
data EndoId  = S | L1 | L2 | I deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "Switch"
         , cite = Citation (Just "p.~16") "weslake2015partial"}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        S -> f (Exo US)
        L1 -> not (e S)
        L2 -> e S
        I -> e L1 || e L2 
        where f = model u i
              e = (f . Endo)

m u i (id) = model u i (Endo id)

u US = True

phi = [(I,True)]
r= Set.fromList [S,L1,L2,I]
