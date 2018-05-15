module Examples.MPollution80
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
import qualified Data.List as List
import Examples.Name

data ExoId  = UA | UB  deriving Show
data EndoId  = A | B | D deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "Pollution, $k=80$"
         , cite = Citation (Just "Ex.~3.11") "DBLP:conf/ijcai/Halpern15"}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        A -> f (Exo UA)
        B -> f (Exo UB) 
        D -> if e A == True then True  --FISH DIE 
                            else False --FISH SURVIVE                                   
        where f  = model u i
              e  = f . Endo

m u i (id) = model u i (Endo id)

--Fish die at 80 kg
u UA = True --100 kg
u UB = True --60 kg

phi = [(D,True)]
r= Set.fromList [A,B,D]
