module Examples.MVote
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

data ExoId  = U1 | U2 | U3 | U4 | U5 | U6 | U7 deriving Show
data EndoId  = V1 | V2 | V3 | V4 | V5 | V6 | V7 | O deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "Vote 5:2"
         , cite = Footnote "Majority vote with 7 participants, 5 of which vote Yea, highlighting the difference between notions based on sufficiency and necessity: 4 Yeas suffice for $O=1$, but if two voters would switch to Nay, the vote would be overturned."}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        V1 -> f (Exo U1)
        V2 -> f (Exo U2)
        V3 -> f (Exo U3)
        V4 -> f (Exo U4)
        V5 -> f (Exo U5)
        V6 -> f (Exo U6)
        V7 -> f (Exo U7)
        O  -> (length [v | v <- vs, v == True ] > 3)
        where f  = model u i
              vs = map (f . Endo) [ V1, V2, V3, V4, V5, V6, V7]

m u i (id) = model u i (Endo id)

u U1 = True
u U2 = True
u U3 = True
u U4 = True
u U5 = True
u _ = False

phi = [(O,True)]
r= Set.fromList [V1,V2,V3,V4,V5,V6,V7]
