module Main where

import Parser
import Scanner
import FlowGraph
import Datatypes
import Worklist
import IA


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
	putStrLn("\nFlow Graph: \n" ++ show(flowGraph))
	let trans = [(AssignType,IAFunction),(ArrayAssignType,IAFunction),(SkipType,NoOp),(BooleanActType,NoOp)]
	let extval = IAExtVal
	let bottom = IAanalysis (Set.empty)
	let ianalysis = worklistInit vertexList [1] extval bottom flowGraph
	putStrLn("\nInitial analysis: \n" ++ (showAnalysis ianalysis 1)) 
	let fanalysis = worklistWork edgeList ianalysis trans flowGraph
	putStrLn("\nFinal entry analysis: \n" ++ (showAnalysis fanalysis 1))
	let fxanalysis = getExitAnalysis (nodes flowGraph) fanalysis trans flowGraph	
	--putStrLn(show fxanalysis)
	putStrLn("\nFinal exit analysis: \n" ++ (showAnalysis fxanalysis 1))
	
	print "done"
