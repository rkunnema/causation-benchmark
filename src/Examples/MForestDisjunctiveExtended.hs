module Examples.MForestDisjunctiveExtended
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

-- data Val = Bottom | Middle | Top | On | Off deriving (Show, Eq, Ord, Enum, Bounded)
data ExoId  = UMD | UL deriving Show
data EndoId  = MD | L | A | B | C | FF deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "--- disjunctive, ext."
         , cite = Citation (Just "Ex. 3.7") "DBLP:conf/ijcai/Halpern15"}
-- control flow variables, although the model is strictly speaking not an extended model

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        MD -> f (Exo UMD)
        L -> f (Exo UL)
        A -> e L && not (e MD)
        B -> not (e L) && e MD
        C -> e L && e MD
        FF -> e A || e B || e C
        where f = model u i
              e = (f . Endo)

m u i (id) = model u i (Endo id)

u UMD = True
u UL  = True

phi = [(FF,True)]
r= Set.fromList [MD,L,FF]
