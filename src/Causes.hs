-- Definition of notion of causes
module Causes  
( emptyi  
, eval  
, update  
, necessary_causes  
, sufficient_causes_open  
, sufficient_causes  
, actual_causes_2015
, cfpSufficient
) where  

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import Debug.Trace


-- The empty intervention: we need this a lot.
emptyi _ = Nothing

--- Helper function: evaluate list of variables as conjunction
eval f [] = True
eval f ((x,y):xys) = (((f x)==y) && (eval f xys))

--- Helper function: update injection
update f x y = (\z -> if z==x then Just y else (f z))

--- Helper function: given candidate list of variables x 
-- create all possible injections, i.e., 
-- functions from variables in c to some values
-- all_i :: (Eq t, Enum t, Bounded t) => [EndoId] -> [EndoId -> Maybe t]
all_i []     = [emptyi]
all_i (c:cs) = concatMap (\i ->  map (update i c) values) (all_i cs)
                    where 
                        values = enumFrom (minBound)

--- Helper function: remove non-minimal sets from list by set inclusion order
removeNonMinimal l = foldr remove l l -- fold complete list with itself ..
    where 
    -- remove x xl | trace ("remove " ++ show x ++ show xl) False = undefined
    remove x xl = [x' | x'<- xl , not( Set.isSubsetOf x x') || x == x' ] 
    --- removing all elements from the list xl at each step if they are a subset of x

--- Helper function: print intervention
print i = concatMap (f) $ enumFrom (minBound)
    where f x = case (i x) of Nothing -> ""
                              Just y  -> show x++"->"++show y++" "

--- Helper function: check if phi holds for all possible injections within some
-- model and context m u
holds_for_all m u phi [] = True 
holds_for_all m u phi (i:is) = ( (eval (m u i) phi) && holds_for_all m u phi is)
-- holds_for_all m u phi (i:is) =  if eval (m u i) phi then 
--                                     holds_for_all m u phi is
--                                 else 
--                                     trace ("Counterexample:"++(Causes.print i))
--                                     False 

-- list of necessary causes (m,u) for list of variables phi
-- (as in all cause definition (m,u) |= X=x for X=x to be a cause)
necessary_causes m u phi = 
-- Idea: look for smallest causes first 
-- and validate NC2.
-- need to check for minimality afterwards nevertheless
        List.nub $ removeNonMinimal (nec Set.empty)
    where 
          vars  = Set.fromAscList $ enumFrom (minBound)
          -- nec c | trace ("nec " ++ show c) False = undefined
          nec c = 
            if (holds_for_all m u phi (all_i (Set.elems c))) then 
                Set.foldr (\c' z -> (nec (Set.insert c' c))++z) [] (Set.difference vars c)
            else
                [c] -- phi does not hold, return candidate as necessary cause

sufficient_causes restr m u phi  = 
        List.nub $ removeNonMinimal (suf Set.empty)
    where 
          -- suf c | trace ("suf " ++ show c) False = undefined
          suf c = 
            if (holds_for_all m u phi (all_i (Set.elems (Set.difference restr c)))) then 
                -- trace "Yes!"
                [c] 
            else
                Set.foldr (\c' z -> (suf (Set.insert c' c))++z) [] (Set.difference restr c)

sufficient_causes_open :: (Enum a0, Eq a0, Ord a0, Ord a1, Show a1, Enum a1, Bounded a1, Show a2, Eq a2, Bounded a2, Enum a2) => (t1 -> (a1 -> Maybe a2) -> t -> a0) -> t1 -> [(t, a0)] -> [Set a1]
sufficient_causes_open = sufficient_causes (Set.fromAscList $ enumFrom (minBound))

--- actual causes

-- Helper function: update i to assign each variable in s' its actual value, i.e., generate W
update_actual m u i s' = List.foldl (\i' c -> update i' c (m u emptyi c) ) i (Set.elems s')

-- Helper functions: power set (list of subsets)
list_of_immeadeate_subsets s = List.map (\e -> Set.delete e s) (Set.elems s)
subsets s = Set.fromList (subs s)
    where subs s = s:(concatMap subs (list_of_immeadeate_subsets s))

all_i_and_wittness m u v c = -- compute all interventions for variables v for candidate cause c
            concatMap candidate_ws is
            where
                w  = Set.difference v c
                is = all_i (Set.elems c)
                candidate_ws i = -- trace (show $subsets w)
                                 map (update_actual m u i) (Set.elems $ subsets w)
                
            
actual_causes_2015 m u phi = 
-- Idea: look for smallest causes first 
-- and validate NC2.
-- need to check for minimality afterwards nevertheless
        List.nub $ removeNonMinimal (act Set.empty)
    where 
          vars  = Set.fromAscList $ enumFrom minBound
          -- act c | trace ("act " ++ show c) False = undefined
          act c = 
            if holds_for_all m u phi (all_i_and_wittness m u vars c) then 
                Set.foldr (\c' z -> (act (Set.insert c' c))++z) [] (Set.difference vars c)
            else
                -- trace "Yes"
                [c] -- phi does not hold, return candidate as necessary cause

cfpSufficient
  :: (Show a1, Bounded t, Bounded a, Bounded a1, Enum t, Enum a1, Eq a,
      Ord t, Show t) =>
     Set t
     -> (t1 -> (t -> Maybe a1) -> t -> a) -> t1 -> [(t, a)] -> [Set t]
cfpSufficient r m u phi =  
        List.nub $ removeNonMinimal (suf Set.empty)
    where 
          vars  = Set.fromAscList $ enumFrom minBound
          ctl  = Set.difference vars r -- assume all unrestricted variables are control variables
          setC = Set.filter (\c -> m u emptyi c /= minBound) ctl
          notC = Set.difference ctl setC
          forbid_cf i z = if Set.member z notC then Just minBound else i z
          allInterventions c = all_i (Set.elems (Set.difference r c))
          -- minBound (e.g., False for Bool) is by convention the indicator
          -- that the control flow variable is deactivated
          -- suf c | trace ("cfpSufficient (notC:" ++ show notC ++ "):"++ show c) False = undefined
          suf c = 
            if holds_for_all m u phi (List.map forbid_cf (allInterventions c)) then 
                -- trace "Yes"
                [c] 
            else
                Set.foldr (\c' z -> suf (Set.insert c' c)++z) [] (Set.difference r c)

