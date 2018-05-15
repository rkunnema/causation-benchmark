module Examples.MCommand
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

data Value = None | Charge | Halt deriving (Show, Eq, Ord, Enum, Bounded) -- minBound::Value should give None value for control flow variable
data ExoId  = UM | US deriving Show
data EndoId  = M | S | C  deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n= Name { name = "Command"
        , cite = Citation (Just "Ex.~8") "weslake2015partial"}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        M -> f (Exo UM)
        S -> f (Exo US)
        C -> if (e M == Charge) || ((e S == Charge) && (not(e M == Halt)))
             then Charge
             else None
        where f  = model u i
              e  = (f . Endo)

m u i id = model u i (Endo id)

u UM = Charge
u US = Charge

phi = [(C,Charge)]
r= Set.fromList [M,S,C]
