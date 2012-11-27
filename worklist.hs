module Worklist where


import Parser
import Scanner
import FlowGraph
import Datatypes
import RD
import AE
import LV
import DS
import Reaches
import IA

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Data.Set (Set)
import Data.List
import qualified Data.Set as Set
import Debug.Trace

-- Extracts a function from a Maybe function, used to deal with the result of the lookup function
extractFunction :: Maybe Function -> Function
extractFunction (Just a) = a
--should never match this one
extractFunction _ = ErrorFunct 

-- Applies a function to a node of the flowgraph
applyFunct :: FlowGraph -> Function -> Analysis -> Label -> Analysis
applyFunct fg IAFunction (IAanalysis set) l = IAanalysis (exitia fg set l)
applyFunct fg RDFunction (RDanalysis set) l = RDanalysis (exitrd fg set l)
applyFunct fg DSFunction (DSanalysis set) l = DSanalysis (exitds fg set l)
applyFunct fg AEFunction (AEanalysis set) l = AEanalysis (exitae fg set l)
applyFunct fg LVFunction (LVanalysis set) l = LVanalysis (exitlv fg set l)
applyFunct fg REFunction (REanalysis set) l = 
	
	{-trace ("\ncalling with entrylist: " ++ show(entryList) ++", \nprogramAexp: " ++ show(programAexp) ++ ", \nlabel: " ++ show(l))-} (REanalysis (exitreaches fg entryList programAexp l) )
	where 
		vertexList = labNodes fg		
		programAexp = Set.toList (aexp vertexList)
		entryList = Set.toList set
	
applyFunct _ NoOp a _ = a 
applyFunct _ _ _ _ = ErrorAnalysis	 		

-- Looks up for the function that applies to the action and then applies this function to it
applyTransFunct :: Label -> Analysis -> [TransFunct] -> FlowGraph -> Analysis
applyTransFunct l a transFunct flowGraph = result
	where
		(_,_,action,_) = context flowGraph l
		actionType = getActType action
		function = extractFunction (lookup actionType transFunct)
		result = {-trace ("\ncalling with analysis: " ++ show(a) ++", label: " ++ show(l))-} (applyFunct flowGraph function a l)

-- Used to apply the extremal value to the extremal labels
applyExtVal :: ExtVal -> [(Node, Action)] -> [Analysis]
applyExtVal (RDExtVal) vertexList = rdExtVal vertexList
applyExtVal (LVExtVal) vertexList = [LVanalysis Set.empty]--lvExtVal vertexList
applyExtVal (AEExtVal) _ = [AEanalysis Set.empty]
applyExtVal (REExtVal) _ = [REanalysis Set.empty]
applyExtVal (DSExtVal) vertexList = dsExtVal vertexList 
applyExtVal (IAExtVal) vertexList = iaExtVal vertexList 

-- Used to apply the extremal value to the extremal labels
applyBottom :: Bottom -> [(Node, Action)] -> [Analysis]
applyBottom (AEInitVal) vertexList = [AEanalysis (aexp vertexList)] 
applyBottom  bottom _ = [bottom]

-- replaces the nth element of a analysis list
replaceNth :: Int -> Analysis -> [Analysis] -> [Analysis]
replaceNth n newVal (x:xs)
     | n == 1 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

-- compares two analysis with the subsetof operator
compareAnalysis :: Analysis -> Analysis -> Bool
compareAnalysis (RDanalysis a1) (RDanalysis a2) =
	if Set.isSubsetOf a1 a2 
		then False
		else True
compareAnalysis (DSanalysis a1) (DSanalysis a2) =
	if Set.isSubsetOf a1 a2 
		then False
		else True
compareAnalysis (IAanalysis a1) (IAanalysis a2) =
	if Set.isSubsetOf a1 a2 
		then False
		else True
compareAnalysis (LVanalysis a1) (LVanalysis a2) =
	if Set.isSubsetOf a1 a2 
		then False
		else True
compareAnalysis (AEanalysis a1) (AEanalysis a2) =
	if Set.isSubsetOf a2 a1
		then False
		else True
compareAnalysis (REanalysis a1) (REanalysis a2) =
	if Set.isSubsetOf a1 a2 
		then False
		else True


-- returns the union of two analysis
analysisLUB :: Analysis -> Analysis -> Analysis
analysisLUB (RDanalysis a1) (RDanalysis a2) = RDanalysis (Set.union a1 a2)
analysisLUB (LVanalysis a1) (LVanalysis a2) = LVanalysis (Set.union a1 a2)
analysisLUB (DSanalysis a1) (DSanalysis a2) = DSanalysis (unionSign (Set.toList a2) (Set.toList a1))
analysisLUB (IAanalysis a1) (IAanalysis a2) = IAanalysis (unionInterval (Set.toList a2) (Set.toList a1))
analysisLUB (REanalysis a1) (REanalysis a2) = REanalysis (Set.union a1 a2) 
analysisLUB (AEanalysis a1) (AEanalysis a2) = AEanalysis (Set.intersection a1 a2)

-- first part of the worklist algorithm, where the extremal labels are initialized to extremal values
worklistInit :: [(Node, Action)] -> ExtLab -> ExtVal -> Bottom -> FlowGraph -> [Analysis]
worklistInit [] _ _ _ _ = []
worklistInit ((n,a):tail) extlab extval bottom fg = result
	where	vertexList = labNodes fg
		analysis = if elem n extlab then applyExtVal extval vertexList else applyBottom bottom vertexList
		rest = worklistInit tail extlab extval bottom fg
		result = analysis ++ rest

-- second part of the worklist algorithm, where the actual work is done
worklistWork :: Worklist -> [Analysis] -> [TransFunct] -> FlowGraph -> [Analysis]
worklistWork [] analysis _ _ = analysis
worklistWork ((n1,n2,()):tail) analysis trans fg = newAnalysisList
	where 	a1=analysis!!(n1-1)
		a2=analysis!!(n2-1)
		a3=applyTransFunct n1 a1 trans fg
		newElement = analysisLUB a2 a3
		newAnalysis = replaceNth n2 newElement analysis
		outEdges = out fg n2  				 				
		newWorklist = Data.List.union outEdges tail	
		newAnalysisList = if compareAnalysis a3 a2
					then worklistWork newWorklist newAnalysis trans fg 
					else worklistWork tail analysis trans fg
	
getExitAnalysis :: [Label] -> [Analysis] -> [TransFunct] -> FlowGraph -> [Analysis]
-- last part of the worklist algorithm, where we apply the exit function to all the entry analysis
getExitAnalysis [] _ _ _ = []
getExitAnalysis (x:xs) analysis trans fg = 
	let	a1=analysis!!(x-1)
		a2=applyTransFunct x a1 trans fg
	in	[a2] ++ (getExitAnalysis xs analysis trans fg) 
	
