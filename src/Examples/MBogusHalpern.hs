module Examples.MBogusHalpern
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

data ExoId  = UP | UA  deriving Show
data EndoId  = P | A | S | PN deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "--- ad-hoc"
         , cite = Citation (Just "p. 29") "DBLP:journals/corr/HalpernH13"}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        P -> f (Exo UP)
        A -> f (Exo UA)
        PN -> e P && e A -- poison neutralized 
        S ->  not (e P) || e PN -- surival if poison not administered or neutralized 
        where f  = model u i
              e = (f . Endo)

m u i (id) = model u i (Endo id)

u UP = False
u UA = True

phi = [(S,True)]
r= Set.fromList [P,A,S]
