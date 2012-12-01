module Main where

import Parser
import Scanner
import FlowGraph
import Datatypes
import Worklist
import DS
import Helper

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
	putStrLn ("\nPARSE TREE: " ++ show(parseTree))
	let (vertexList,edgeList,_) = recursiveFG mergedList [] [] 1 1
	putStrLn ("\nVERTEX LIST: \n" ++ showVertexList(vertexList))
	putStrLn ("\nEDGE LIST: \n" ++ show(edgeList))	
	let flowGraph :: Gr Action () = mkGraph vertexList edgeList    
	putStrLn("\nFLOW GRAPH: " ++ show(flowGraph))
	let trans = [(AssignType,DSFunction),(ArrayAssignType,DSFunction),(ReadActType, DSFunction),(ReadArrayType,DSFunction),(SkipType,NoOp),(BooleanActType,NoOp),(WriteActType,DSFunction)]
	let extval = DSExtVal
	let bottom = DSanalysis (Set.empty)
	putStrLn("\n---------------------------")	
	putStrLn("DETECTION OF SIGNS ANALYSIS")
	putStrLn("-----------------------------")		
	let ianalysis = worklistInit vertexList [1] extval bottom flowGraph
	putStrLn("\nInitial analysis: \n" ++ (showAnalysis ianalysis 1)) 
	let fanalysis = worklistWork edgeList ianalysis trans flowGraph
	putStrLn("\nFinal entry analysis: \n" ++ (showAnalysis fanalysis 1))
	let fxanalysis = getExitAnalysis (nodes flowGraph) fanalysis trans flowGraph	
	putStrLn("\nFinal exit analysis: \n" ++ (showAnalysis fxanalysis 1))
	
	print "done"
