module Examples.MTrainCtl
(
      n
    , m
    , u
    , phi
    , r
)
where

-- train example from hall structural equations and causation

import Prelude
import Data.Set (Set)
import qualified Data.Set as Set
import Examples.Name

data ExoId  = UF  deriving Show
data EndoId = F | L_r | R_r | A  deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "--- (ctl)"
         , cite = Footnote "Model with $\\Vrch=\\set{L_r,R_r}$, where $L_r=\\top$ iff $\\neg F$, $\\mathit{R}_r=\\top$ iff $F$ and $A=1$ iff $L_r=\\top$ or $R_r=\\top$, allowing train to get stuck."}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        F -> f (Exo UF)
        L_r -> not (e F) -- flip shows left = False
        R_r -> e F
        A -> if (e L_r) || (e R_r) then
                True
            else 
                False -- could also set to True to say the train always arrives.
        where f  = model u i
              e  = (f . Endo)

m u i (id) = model u i (Endo id)

u UF = True

phi = [(A,True)]
r= Set.fromList [F,A]
