module Main where

import Parser
import Scanner
import Data.Graph.Inductive
import Data.Graph.Inductive.Graph


flowGraph :: Gr Action ()
flowGraph = empty

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
	let flowGraph' :: Gr Action () = mkGraph vertexList edgeList    
	putStrLn("Flow Graph: " ++ show(flowGraph'))
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

-- create graph takes a list of LabelStatement and a Graph and an Integer and returns a Graph
createGraph :: [Action] -> Gr Action () -> Int -> Gr Action ()
createGraph (x:xs) g 1 = createGraph xs (([],1,x,[]) & g) 2
createGraph (x:xs) g n = createGraph xs (([((),	(n-1))],n,x,[]) & g) (n+1) 
createGraph [] g n = g

headStatement :: StmtList -> Stmt
headStatement (StmtList stmt stmtList) = stmt 

tailStatement :: StmtList -> StmtList
tailStatement (StmtList stmt stmtList) = stmtList
tailStatement NoStmt = NoStmt

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

recursiveFG (StmtList stmt stmtlist) vertexList edgeList lc edgeHead = 
	let 	action = getAction(stmt)		
		newVertexList = vertexList ++ [(lc, action)]
		newEdge = createEdgeList edgeHead lc		
		newEdgeList = edgeList ++ newEdge		
		g = recursiveFG stmtlist newVertexList newEdgeList (lc+1) lc
 		
	in	g

recursiveFG NoStmt vertexList edgeList lc edgeHead = (vertexList, edgeList, lc) 
		
			 



-- takes a StmtList and returns a list of LabelStatements
--parseStmtList :: StmtList -> [LabelStatement]
--parseStmtList (StmtList stmt stmtList) = parseStmt(stmt):parseStmtList(stmtList)
--parseStmtList (NoStmt) = []
--parseStmt :: Stmt -> LabelStatement
--parseStmt (StmtAssign i a) = SAssign {identifier=i, aexpr=a}


