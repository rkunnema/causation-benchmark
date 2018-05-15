module Examples.MBackupCtl
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

data ExoId  = UT deriving Show
data EndoId  = T | S | V | T_r | NT_r | S_r  deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "--- (ctl)"
         , cite = Citation (Just "Ex.~1") "weslake2015partial"}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        T -> f (Exo UT)
	NT_r -> not (e T)
	T_r -> e T
	S -> e NT_r
        S_r -> e S
        V -> (e T_r) || (e S_r)
        where f  = model u i
              e  = (f . Endo)

m u i id = model u i (Endo id)

u UT = True

phi = [(V,True)]
r= Set.fromList [T,S,V]
