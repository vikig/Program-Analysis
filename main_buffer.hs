module Main where

import Parser
import Scanner
import FlowGraph
import Datatypes
import Worklist
import IA
import BufferOverflow
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
	putStrLn("\n----------")	
	putStrLn("FLOW GRAPH")
	putStrLn("----------")		
	let (vertexList,edgeList,_) = recursiveFG mergedList [] [] 1 1
	putStrLn ("\nVERTEX LIST: \n" ++ showVertexList(vertexList))
	putStrLn ("\nEDGE LIST: \n" ++ show(edgeList))	
	let flowGraph :: Gr Action () = mkGraph vertexList edgeList    
	let trans = [(AssignType,IAFunction),(ArrayAssignType,IAFunction),(ReadActType, IAFunction),(ReadArrayType,IAFunction),(SkipType,NoOp),(BooleanActType,NoOp),(WriteActType,NoOp)]
	let extval = IAExtVal
	let bottom = IAanalysis (Set.empty)
	let ianalysis = worklistInit vertexList [1] extval bottom flowGraph
	putStrLn("\n-----------------")	
	putStrLn("INTERVAL ANALYSIS")
	putStrLn("-----------------")		
	putStrLn("\nInitial analysis: \n" ++ (showAnalysis ianalysis 1)) 
	let fanalysis = worklistWork edgeList ianalysis trans flowGraph declList
	putStrLn("\nFinal Entry Analysis: \n" ++ (showAnalysis fanalysis 1))
	let fxanalysis = getExitAnalysis (nodes flowGraph) fanalysis trans flowGraph declList	
	putStrLn("\nFinal Exit Analysis: \n" ++ (showAnalysis fxanalysis 1))
	let overflow =  if (checkIfErrorAnalysis fanalysis) 
				then error("Interval Analysis contains error states") 
				else bufferOverflow declList fanalysis vertexList
	let output = showOverflowAnalysis(overflow) 
	putStrLn("\n-------------------------")		
	putStrLn("BUFFER OVERFLOW ANALYSIS:") 
	putStrLn("-------------------------\n")	
	putStrLn(if output==[] then "No Array Assignments\n" else output)
	
	print "done"
