module Main where

import Parser
import Scanner
import FlowGraph
import Datatypes
import RD
import Worklist

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
