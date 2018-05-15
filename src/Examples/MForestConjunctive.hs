-- Model description: conjunctive forest fire
module Examples.MForestConjunctive
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

data ExoId  = UA | UB deriving Show
data EndoId  = A | B | FF deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "Forest fire"
         , cite = Example "ex:forest-conjunctive"}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        A -> f (Exo UA)
        B -> f (Exo UB)
        FF -> (f $Endo A) && (f $Endo B)
        where f = model u i

m u i (id) = model u i (Endo id)

u UA = True
u UB = True

phi = [(FF,True)]

r = Set.fromList [A,B,FF]
