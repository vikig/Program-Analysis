module FlowGraph where

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Parser
import Datatypes
import Debug.Trace

	

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

mergeStmtDecl :: DeclList -> StmtList -> StmtList
mergeStmtDecl (DeclList (DeclArray i 1) declList) sl = 
		let 	s :: Stmt = StmtAssignArray i (Aexpr1(Aexpr2(Aexpr3 (IntegerLiteral 0)))) (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral 0))))
			sl' :: StmtList = mergeStmtDecl declList sl
			r = StmtList s sl'		
		in r
mergeStmtDecl (DeclList (DeclArray i a) declList) sl = 
		let 	s :: Stmt = StmtAssignArray i (Aexpr1(Aexpr2(Aexpr3 (IntegerLiteral (a-1))))) (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral 0))))
			sl' :: StmtList = mergeStmtDecl (DeclList (DeclArray i (a-1)) declList) sl
			r = StmtList s sl'		
		in r
mergeStmtDecl (DeclList (Decl i) declList) sl = 
		let 	s :: Stmt = StmtAssign i (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral 0))))
			sl' :: StmtList = mergeStmtDecl declList sl
			r = StmtList s sl'		
		in r

mergeStmtDecl NoDecl sl = sl 

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

getEnclosingBoolean :: FlowGraph -> Node -> Int -> Int -> Node
getEnclosingBoolean fg node ignoreBool flag = result
	where
			vertexList = labNodes fg
			predecessor = pre fg node
			ignoreNumber = (parsePredecessor predecessor node) + ignoreBool
			(n,a) = if (flag==0)
					then vertexList!!((minimum predecessor)-1)
					else vertexList!!((maximum predecessor)-1) 			
			result = 
				if ((predecessor == []) || ((minimum predecessor)>node)) 
					then 	(-1)
					else 	if (checkIfBoolean a) 
							then 
								if ((ignoreNumber>0) && (length(suc fg n)==2)) 
									then getEnclosingBoolean fg n (ignoreNumber-1) flag 
									else n
							else (getEnclosingBoolean fg n ignoreNumber flag)
			

parsePredecessor :: [Node] -> Node -> Int
parsePredecessor [] _ = 0
parsePredecessor pre node =
	let	le = length pre
	in	case le of
			1 -> 0
			2 -> if ((maximum pre)>node) then 0 else 1
			_ -> ceiling((toRational(le))/2)

checkIfBoolean :: Action -> Bool
checkIfBoolean (BooleanAct _) = True
checkIfBoolean _ = False
			


		

