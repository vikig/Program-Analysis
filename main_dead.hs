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
import Helper

main :: IO ()
main = do
	inStr <- getContents
	let parseTree = testpar (alexScanTokens inStr)
	let declList = getDeclList(parseTree)	
	let statementList = getStmtList(parseTree)
	let mergedList = mergeStmtDecl declList statementList  
	putStrLn("\n----------")	
	putStrLn("FLOW GRAPH")
	putStrLn("----------")		
	let (vertexList,edgeList,_) = recursiveFG mergedList [] [] 1 1
	putStrLn ("\nVERTEX LIST: \n" ++ showVertexList(vertexList))
	putStrLn ("\nEDGE LIST: \n" ++ show(edgeList))	
	let flowGraph :: Gr Action () = mkGraph vertexList edgeList    
	let reverseFG = grev flowGraph
	let edgeRev = labEdges reverseFG
	let trans = [(AssignType,LVFunction),(ArrayAssignType,LVFunction),(SkipType,NoOp),(BooleanActType,LVFunction),(WriteActType,LVFunction),(ReadActType,LVFunction),(ReadArrayType,LVFunction)]
	let extval = LVExtVal
	let bottom = LVanalysis (Set.empty)
	putStrLn("\n-----------------------")	
	putStrLn("LIVE VARIABLES ANALYSIS")
	putStrLn("-----------------------")
	let ianalysis = worklistInit vertexList [noNodes flowGraph] extval bottom reverseFG
	putStrLn("\nInitial analysis: \n" ++ (showAnalysis ianalysis 1)) 
	let fanalysis = worklistWork (reverse edgeRev) ianalysis trans reverseFG declList
	putStrLn("\nFinal entry analysis: \n" ++ (showAnalysis fanalysis 1))
	let fxanalysis = getExitAnalysis (nodes flowGraph) fanalysis trans reverseFG declList
	putStrLn("\nFinal exit analysis: \n" ++ (showAnalysis fxanalysis 1))
	let deadlabels = deadCode (nodes flowGraph) fanalysis vertexList
	putStrLn("\nDEAD LABELS: \n" ++ show(deadlabels))
	
	print "done"
