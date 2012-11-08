module Main where

import Parser
import Scanner
import Data.Graph.Inductive
import Data.Graph.Inductive.Graph


data Action = 
	Assign {	
	identifier 	:: Identifier,
	value		:: Aexpr
	}
	|
	ArrayAssign {	
	arrayname 	:: Identifier,
	index		:: Aexpr,
	value 		:: Aexpr
	}
	|
	BooleanAct {
	boolean		:: Bexpr
	}
	|
	WriteAct {
	aexpr	:: Aexpr
	}
	|
	ReadAct {
	variable	:: Identifier
	}
	|	
	ReadArray {
	arrayname 	:: Identifier,
	index		:: Aexpr	 
	}
	|
	Skip	
	deriving(Show, Eq)	


main :: IO ()
main = do
	inStr <- getContents
	let parseTree = testpar (alexScanTokens inStr)
	let statementList = getStmtList(parseTree)
	putStrLn ("Parse Tree: " ++ show(parseTree))
	putStrLn ("Statement List: " ++ show(statementList))
	let (vertexList,edgeList,_) = recursiveFG statementList [] [] 1	1
	putStrLn ("V: " ++ show(vertexList))
	putStrLn ("E: " ++ show(edgeList))	
	let flowGraph :: Gr Action () = mkGraph vertexList edgeList    
	putStrLn("Flow Graph: " ++ show(flowGraph))
	print "done"

getAction :: Stmt -> Action
getAction (StmtAssign i e) = Assign {identifier=i, value=e} 
getAction (StmtAssignArray i n e) = ArrayAssign {arrayname=i, index=n, value=e}
getAction (StmtSkip) = Skip
getAction (StmtIf b sl1 sl2) = BooleanAct {boolean=b}
getAction (StmtRead i) = ReadAct {variable=i}
getAction (StmtReadArray a i) = ReadArray {arrayname=a, index=i}
getAction (StmtWrite i) = WriteAct {aexpr=i}
getAction (StmtWhile b sl) = BooleanAct {boolean=b}

createEdgeList :: Node -> Node -> [UEdge]
createEdgeList 1 1 = []
createEdgeList edgeHead lc = [(edgeHead, lc, ())]

recursiveFG :: StmtList -> [(Node, Action)] -> [UEdge] -> Node -> Node -> ([(Node, Action)],[UEdge],Node)
recursiveFG (StmtList (StmtWhile bexpr stmtlist') stmtlist) vertexList edgeList lc edgeHead = 
	let 	action = getAction(StmtWhile bexpr stmtlist')		
		newVertexList = vertexList ++ [(lc, action)]
		newEdge = createEdgeList edgeHead lc		
		newEdgeList = edgeList ++ newEdge
		(newVertexList', newEdgeList', lc') = recursiveFG stmtlist' newVertexList newEdgeList (lc+1) lc 		
		newEdge' = createEdgeList (lc'-1) lc
		newEdgeList'' = newEdgeList' ++ newEdge'		
		g = recursiveFG stmtlist newVertexList' newEdgeList'' lc' lc
 		
	in	g

recursiveFG (StmtList (StmtIf bexpr thenStmtList elseStmtList) NoStmt) vertexList edgeList lc edgeHead =
	let	
		-- Build the sub graph from StmtIf
		(newVertexList, newEdgeList, lc1, lc2) = stmtIfFG (StmtIf bexpr thenStmtList elseStmtList) vertexList edgeList lc edgeHead
		-- Return the graph from newVertextList, newEdgeList
		g = recursiveFG NoStmt newVertexList newEdgeList lc2 (lc2 - 1)
		
	in 	g

recursiveFG (StmtList (StmtIf bexpr thenStmtList elseStmtList) stmtList) vertexList edgeList lc edgeHead =
	let	
		-- Build the sub graph from StmtIf
		(newVertexList, newEdgeList, lc1, lc2) = stmtIfFG (StmtIf bexpr thenStmtList elseStmtList) vertexList edgeList lc edgeHead
		-- Create the edge from the last "then" stmt to the stmtList
		newEdge = createEdgeList (lc1 - 1) lc2
		-- Build the graph from the stmtList, consider the last "else" stmt to be the edgeHead
		g = recursiveFG stmtList newVertexList (newEdgeList ++ newEdge) lc2 (lc2 - 1)
		
	in 	g

recursiveFG (StmtList stmt stmtlist) vertexList edgeList lc edgeHead = 
	let 	action = getAction(stmt)		
		newVertexList = vertexList ++ [(lc, action)]
		newEdge = createEdgeList edgeHead lc		
		newEdgeList = edgeList ++ newEdge		
		g = recursiveFG stmtlist newVertexList newEdgeList (lc+1) lc
 		
	in	g

recursiveFG NoStmt vertexList edgeList lc edgeHead = (vertexList, edgeList, lc)

stmtIfFG :: Stmt -> [(Node, Action)] -> [UEdge] -> Node -> Node -> ([(Node, Action)],[UEdge],Node,Node)
stmtIfFG (StmtIf bexpr thenStmtList elseStmtList) vertexList edgeList lc edgeHead =
	let	-- Add the node of bexpr and the edge of this node and the last node in the graph
		action = getAction(StmtIf bexpr thenStmtList elseStmtList)
		newVertexList = vertexList ++ [(lc, action)]
		newEdge = createEdgeList edgeHead lc		
		newEdgeList = edgeList ++ newEdge
		-- Build the graph from then "then" stmtList
		(newVertexList1, newEdgeList1, lc1) = recursiveFG thenStmtList newVertexList newEdgeList (lc+1) lc 		
		-- Build the graph from the "else" stmtList
		(newVertexList2, newEdgeList2, lc2) = recursiveFG elseStmtList newVertexList1 newEdgeList1 lc1 lc 	
		g = (newVertexList2, newEdgeList2, lc1, lc2)
		
	in 	g 
		

