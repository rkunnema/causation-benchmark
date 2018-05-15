module Examples.MBoS
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

data ExoId  = UN  deriving Show
data EndoId  = N | P_1 | P_2 | C | T deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "BoS"
         , cite = Example "ex:bos"}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        N -> f (Exo UN)
        P_1 -> e N
        P_2 -> e N
        C -> e P_1 == e P_2
        T -> e C
        where f  = model u i
              e  = f . Endo

m u i (id) = model u i (Endo id)

u UN = True

phi = [(T,True)]
r= Set.fromList [N,P_1,P_2,T]
