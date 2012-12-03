module Main where

import Parser
import Scanner
import FlowGraph
import Datatypes
import RD
import Worklist
import AE
import Slice
import Helper

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Data.Set (Set)
import Data.List
import qualified Data.Set as Set
import Data.Ord
import System (getArgs)


main :: IO ()
main = do
	args <- getArgs
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
	let trans = [(AssignType,RDFunction),(ArrayAssignType,RDFunction),(ReadActType, RDFunction),(ReadArrayType,RDFunction),(SkipType,NoOp),(BooleanActType,NoOp),(WriteActType,NoOp)]
	let extval = RDExtVal
	let bottom = RDanalysis (Set.empty)
	let ianalysis = worklistInit vertexList [1] extval bottom flowGraph
	putStrLn("\n-----------------------------")	
	putStrLn("REACHING DEFINITIONS ANALYSIS")
	putStrLn("-----------------------------")		
	putStrLn("\nInitial analysis: \n" ++ (showAnalysis ianalysis 1)) 
	let fanalysis = worklistWork edgeList ianalysis trans flowGraph declList
	putStrLn("\nFinal entry analysis: \n" ++ (showAnalysis fanalysis 1))
	let fxanalysis = getExitAnalysis (nodes flowGraph) fanalysis trans flowGraph declList	
	putStrLn("\nFinal exit analysis: \n" ++ (showAnalysis fxanalysis 1))
	let parsed = reads (head args)
	let point = if parsed == [] then error("ARGUMENT IS NOT A NUMBER") else (truncate (fst (head parsed))) 	
	let slice = mainSlice flowGraph fanalysis [point] Set.empty 
	let sliceo = sortBy (comparing fst) slice	
	putStrLn ("\nSLICE AT LABEL "++ show(point)++": \n" ++ showVertexList(sliceo))
	print "done"
