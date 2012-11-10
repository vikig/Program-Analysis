module Main where

import Parser
import Scanner
import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Data.Set (Set)
import qualified Data.Set as Set

type Label = Int

data MonFramework = Lattice Functions Flow (Set Label) ExtVal TransFunct

data Lattice = 
	RD (Set (Identifier, Label)) Subset

data Flow = 
	RD (Set UEdge)

data ExtVal = 
	RD (Identifier -1) -- this should map all the variables to -1

data TransFunct =
	RD ((Identifier, Label) -> (Identifier, Label))



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
	let mergedList = mergeStmtDecl declList statementList  	
	putStrLn ("Parse Tree: " ++ show(parseTree))
	putStrLn ("Statement List: " ++ show(mergedList))
	let (vertexList,edgeList,_) = recursiveFG mergedList [] [] 1 1
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
		

