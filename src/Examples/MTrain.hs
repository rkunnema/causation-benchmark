module Examples.MTrain
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

data ExoId  = UF | ULB | URB deriving Show
data EndoId  = F | LB | RB | A  deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "Train"
         , cite = Citation (Just "Ex.~4") "DBLP:journals/corr/abs-1106-2652"}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        F -> f (Exo UF)
        LB -> f (Exo ULB)
        RB -> f (Exo URB)
        A -> if (e F)==False -- train goes left
             then not (e LB) -- arrives if left is not blocked
             else not (e RB)
        where f  = model u i
              e  = (f . Endo)

m u i (id) = model u i (Endo id)

u UF = True
u ULB = False
u URB = False

phi = [(A,True)]
r= Set.fromList [F,LB,RB,A]
