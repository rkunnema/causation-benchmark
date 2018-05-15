module Examples.MLate
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

data Value = Off | On | S | B | N deriving (Show,Eq,Ord,Enum,Bounded) -- minBound::Value should give Off value for control flow variables
data ExoId  =  UT1 | UT2 | UT3   deriving Show
data EndoId  = T1 | T2 | T3 | BS1 | BS2 | BS3 | BS  deriving (Show, Eq, Ord, Enum, Bounded)
data Var = Exo ExoId | Endo EndoId deriving Show

n = Name { name = "Late preemption"
         , cite = Example "ex:ctl-var"}

model u i (Exo id) = (u id)
model u i (Endo id) = case (i id) of
    Just v -> v
    Nothing -> case id of
        -- This is the only part that should be changed.
        T1 -> f (Exo UT1)
        T2 -> f (Exo UT2)
        T3 -> f (Exo UT3)
        BS1 -> if notN (f (Endo T1)) then On else Off 
        BS2 -> if f' BS1 == Off && notN (f (Endo T2)) then On else Off 
        BS3 -> if f' BS2 == Off && notN (f (Endo T3)) then On else Off 
        BS ->  case List.find (==On) (map f' [BS1,BS2,BS3])  of 
                Just _ -> On
                Nothing -> Off
        where f  = (model u i)
              f' = f . Endo
              notN N = False
              notN _ = True

m u i (id) = model u i (Endo id)

u UT1 = S
u UT2 = B
u _   = N

phi = [(BS,On)]
r = Set.fromList [T1,T2,T3,BS]
