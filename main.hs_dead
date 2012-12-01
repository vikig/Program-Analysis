module Main where

import Parser
import Scanner
import FlowGraph
import Datatypes
import Worklist
import DeadCode


import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Data.Set (Set)
import Data.List
import qualified Data.Set as Set
import Data.Ord


main :: IO ()
main = do
	inStr <- getContents
	let parseTree = testpar (alexScanTokens inStr)
	let declList = getDeclList(parseTree)	
	let statementList = getStmtList(parseTree)
	let mergedList = mergeStmtDecl declList statementList  	
	putStrLn ("\nParse Tree: " ++ show(parseTree))
	--putStrLn ("\nStatement List: " ++ show(mergedList))
	let (vertexList,edgeList,_) = recursiveFG mergedList [] [] 1 1
	putStrLn ("\nVertex List: \n" ++ showVertexList(vertexList))
	putStrLn ("\nEdge List: \n" ++ show(edgeList))	
	let flowGraph :: Gr Action () = mkGraph vertexList edgeList    
	let reverseFG = grev flowGraph
	let edgeRev = labEdges reverseFG
	putStrLn ("\nReverse Flow: \n" ++ show(reverse edgeRev))
	putStrLn("\nFlow Graph: \n" ++ show(flowGraph))
	let trans = [(AssignType,LVFunction),(ArrayAssignType,LVFunction),(SkipType,NoOp),(BooleanActType,LVFunction)]
	let extval = LVExtVal
	let bottom = LVanalysis (Set.empty)
	let ianalysis = worklistInit vertexList [noNodes flowGraph] extval bottom reverseFG
	putStrLn("\nInitial analysis: \n" ++ (showAnalysis ianalysis 1)) 
	let fanalysis = worklistWork (reverse edgeRev) ianalysis trans reverseFG
	putStrLn("\nFinal entry analysis: \n" ++ (showAnalysis fanalysis 1))
	let fxanalysis = getExitAnalysis (nodes flowGraph) fanalysis trans reverseFG	
	putStrLn("\nFinal exit analysis: \n" ++ (showAnalysis fxanalysis 1))
	let deadlabels = deadCode (nodes flowGraph) fanalysis vertexList
	putStrLn("\nDead Labels: \n" ++ show(deadlabels))
	
	print "done"
