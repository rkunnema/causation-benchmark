module Examples.MAgreement
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

data Value = Bot | On | Off deriving (Show, Eq, Ord, Enum, Bounded) -- minBound::Value should give Off value for control flow variable
data ExoId  = UA | UB deriving Show
data EndoId  = A | B | R | O  deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n= Name { name = "Agreement"
        , cite = Example "ex:agreement"}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        A -> f (Exo UA)
        B -> f (Exo UB)
        R -> if e A==e B
             then On
             else Off
        O -> if e R== On
             then e A
             else Bot
        where f  = model u i
              e  = (f . Endo)

m u i id = model u i (Endo id)

u UA = On
u UB = On

phi = [(O,On)]
r= Set.fromList [A,B,O]
