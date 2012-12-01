module Helper where


import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Data.Set (Set)
import qualified Data.Set as Set

import Datatypes

-- returns the variables of the program
freevar :: [(Label, Action)] -> Set Identifier
freevar [] = Set.empty
freevar ((_,(Assign i _)):xs) = u
	where		
		h = Set.singleton i
		t = freevar xs
		u = Set.union h t
freevar ((_,(ArrayAssign i (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral n)))) _)):xs) = u
	where		
		a = i ++ "[" ++ Prelude.show(n) ++ "]" 
		h = Set.singleton a
		t = freevar xs
		u = Set.union h t
freevar (_:xs) = freevar xs

makeTuple :: a -> Identifier -> (Identifier, a)
makeTuple l i = (i,l)

-- Extracts an action from a Maybe action, used to deal with the result of the lookup function
extractAction :: Maybe Action -> Action
extractAction (Just a) = a
--should never match this one
extractAction _ = ErrorAct 
