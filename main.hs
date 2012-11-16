module Main where

import Parser
import Scanner
import FlowGraph
import Datatypes
import RD

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Data.Set (Set)
import Data.List
import qualified Data.Set as Set


main :: IO ()
main = do
	inStr <- getContents
	let parseTree = testpar (alexScanTokens inStr)
	let declList = getDeclList(parseTree)	
	let statementList = getStmtList(parseTree)
	let mergedList = mergeStmtDecl declList statementList  	
	putStrLn ("Parse Tree: " ++ show(parseTree))
	putStrLn ("Statement List: " ++ show(mergedList))
	let (vertexList,edgeList,_) = recursiveFG mergedList [] [] 1 1
	putStrLn ("V: " ++ show(vertexList))
	putStrLn ("E: " ++ show(edgeList))	
	let flowGraph :: Gr Action () = mkGraph vertexList edgeList    
	putStrLn("Flow Graph: " ++ show(flowGraph))
	let trans = [(AssignType,RDFunction),(ArrayAssignType,RDFunction),(SkipType,NoOp),(BooleanActType,NoOp)]
	let extval = RDExtVal
	let bottom = RDanalysis (Set.empty)
	let ianalysis = worklistInit vertexList [1] extval bottom
	putStrLn("Initial analysis: " ++ show(ianalysis)) 
	let fanalysis = worklistWork edgeList ianalysis trans flowGraph
	putStrLn("Final entry analysis: " ++ show(fanalysis))
	let fxanalysis = getExitAnalysis (nodes flowGraph) fanalysis trans flowGraph	
	putStrLn("Final exit analysis: " ++ show(fxanalysis))
	
	print "done"

extractFunction :: Maybe Function -> Function
extractFunction (Just a) = a
--should never match this one
extractFunction _ = ErrorFunct 

applyFunct :: FlowGraph -> Function -> Analysis -> Label -> Analysis
applyFunct fg RDFunction (RDanalysis set) l = RDanalysis (exitrd fg set l)
applyFunct _ NoOp a _ = a 
applyFunct _ _ _ _ = ErrorAnalysis	 		

applyTransFunct :: Label -> Analysis -> [TransFunct] -> FlowGraph -> Analysis
applyTransFunct l a transFunct flowGraph = result
	where
		(_,_,action,_) = context flowGraph l
		actionType = getActType action
		function = extractFunction (lookup actionType transFunct)
		result = applyFunct flowGraph function a l

applyExtVal :: ExtVal -> [(Node, Action)] -> [Analysis]
applyExtVal (RDExtVal) vertexList = rdExtVal vertexList
applyExtVal extval _ = [extval] 


worklistInit :: [(Node, Action)] -> ExtLab -> ExtVal -> Bottom -> [Analysis]
worklistInit [] _ _ _ = []
worklistInit ((n,a):tail) extlab extval bottom = result
	where	analysis = if elem n extlab then applyExtVal extval ((n,a):tail) else [bottom] 
		rest = worklistInit tail extlab extval bottom
		result = analysis ++ rest



		
replaceNth :: Int -> Analysis -> [Analysis] -> [Analysis]
replaceNth n newVal (x:xs)
     | n == 1 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs



compareAnalysis :: Analysis -> Analysis -> Bool
compareAnalysis (RDanalysis a1) (RDanalysis a2) =
	if Set.isSubsetOf a1 a2 
		then False
		else True

analysisUnion :: Analysis -> Analysis -> Analysis
analysisUnion (RDanalysis a1) (RDanalysis a2) = RDanalysis (Set.union a1 a2) 

worklistWork :: Worklist -> [Analysis] -> [TransFunct] -> FlowGraph -> [Analysis]
worklistWork [] analysis _ _ = analysis
worklistWork ((n1,n2,()):tail) analysis trans fg = newAnalysisList
	where 	a1=analysis!!(n1-1)
		a2=analysis!!(n2-1)
		a3=applyTransFunct n1 a1 trans fg
		newElement = analysisUnion a2 a3
		newAnalysis = replaceNth n2 newElement analysis
		outEdges = out fg n2  				 				
		newWorklist = Data.List.union outEdges tail	
		newAnalysisList = if compareAnalysis a3 a2
					then worklistWork newWorklist newAnalysis trans fg 
					else worklistWork tail analysis trans fg

getExitAnalysis :: [Label] -> [Analysis] -> [TransFunct] -> FlowGraph -> [Analysis]
getExitAnalysis [] _ _ _ = []
getExitAnalysis (x:xs) analysis trans fg = 
	let	a1=analysis!!(x-1)
		a2=applyTransFunct x a1 trans fg
	in	[a2] ++ (getExitAnalysis xs analysis trans fg) 
	



