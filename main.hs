module Main where

import Parser
import Scanner
import FlowGraph
import Datatypes
import RD
import Worklist
import AE
import Slice

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
	putStrLn ("\nParse Tree: " ++ show(parseTree))
	putStrLn ("\nStatement List: " ++ show(mergedList))
	let (vertexList,edgeList,_) = recursiveFG mergedList [] [] 1 1
	putStrLn ("\nV: " ++ show(vertexList))
	putStrLn ("\nE: " ++ show(edgeList))	
	let flowGraph :: Gr Action () = mkGraph vertexList edgeList    
	putStrLn("\nFlow Graph: " ++ show(flowGraph))
	let trans = [(AssignType,RDFunction),(ArrayAssignType,RDFunction),(SkipType,NoOp),(BooleanActType,NoOp)]
	let extval = RDExtVal
	let bottom = RDanalysis (Set.empty)
	let ianalysis = worklistInit vertexList [1] extval bottom flowGraph
	putStrLn("\nInitial analysis: \n" ++ (showAnalysis ianalysis 1)) 
	let fanalysis = worklistWork edgeList ianalysis trans flowGraph
	putStrLn("\nFinal entry analysis: \n" ++ (showAnalysis fanalysis 1))
	let fxanalysis = getExitAnalysis (nodes flowGraph) fanalysis trans flowGraph	
	putStrLn("\nFinal exit analysis: \n" ++ (showAnalysis fxanalysis 1))
	let point :: Label = 4	
	let slice = mainSlice vertexList fanalysis [point] Set.empty 
	putStrLn ("\nSlice at label "++ show(point)++": \n" ++ show(slice))
	print "done"
