module DeadCode where



import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Data.Set (Set)
import Data.List
import qualified Data.Set as Set
import Debug.Trace

import LV
import Datatypes

deadCode :: [Label] -> [Analysis] -> VertexList -> [Label]

deadCode [] _ _ = []

deadCode (l:lTail) ((LVanalysis entryAnalysis):aTail) ((n,a):vTail) = result ++ (deadCode lTail aTail vTail)
	where
			killset = killlv a
			result = if Set.isProperSubsetOf entryAnalysis killset
					then	[l]
					else 	[]
