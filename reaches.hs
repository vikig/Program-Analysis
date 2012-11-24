module Reaches where

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map
import Data.List
import Parser
import FlowGraph
import Datatypes
import AE
import Debug.Trace
--Exit analysis of a label
exitreaches	 :: FlowGraph -> [(Aexpr, Label)] -> [Aexpr] -> Label -> Set (Aexpr, Label)
exitreaches _ _ [] _ = Set.empty
exitreaches fg entrySet (h:t) label = {-trace("temp= " ++ show(temp))-} (Set.union temp rest)
	where
		vertexList = labNodes fg	
		action = extractAction (Prelude.lookup label vertexList)		
		temp = {-trace("h: " ++ show(h))-} (exitOneAexpr action h label entrySet) 		
		rest = exitreaches fg entrySet t label

exitOneAexpr :: Action -> Aexpr -> Label -> [(Aexpr, Label)] -> Set (Aexpr, Label)
exitOneAexpr (Assign i v) a l entrylist =
	if v==a
		then
			if (getExpressionsThatContain [a] i)==Set.empty 
				then Set.singleton (a, l)
				else Set.empty
		else	
			if (getExpressionsThatContain [a] i)==Set.empty
				then (Set.fromList entrylist)
				else Set.empty
--exitOneAexpr _ _ _ entryset = Set.fromList entryset
