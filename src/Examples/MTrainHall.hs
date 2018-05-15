module Examples.MTrainHall
(
      n
    , m
    , u
    , phi
    , r
)
where

-- train example from hall structural equations and causation

import Prelude hiding (LT)
import Data.Set (Set)
import qualified Data.Set as Set
import Examples.Name

data ExoId  = UF | ULT | URT deriving Show
data EndoId = F | LT | RT | A  deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "---"
         , cite = Citation Nothing "Hall2000-HALCAT-7"}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        F -> f (Exo UF)
        LT -> f (Exo ULT)
        RT -> f (Exo URT)
        A -> if (e F)==False -- flip shows left
             then e LT -- arrives if left is not blocked
             else e RT
        where f  = model u i
              e  = (f . Endo)

m u i (id) = model u i (Endo id)

u UF = True
u ULT = False
u URT = True

phi = [(A,True)]
r= Set.fromList [F,A]
