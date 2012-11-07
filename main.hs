module Main where

import Parser
import Scanner
import Data.Graph.Inductive
import Data.Graph.Inductive.Graph


flowGraph :: Gr LabelStatement ()
flowGraph = empty

data LabelStatement = 
	SAssign {	
	identifier 	:: Identifier,
	aexpr		:: Aexpr
	}
	|
	CAssign {	
	identifier 	:: Identifier,
	intliteral	:: IntegerLiteral
	}
	deriving(Show, Eq)	


main :: IO ()
main = do
	inStr <- getContents
	let parseTree = testpar (alexScanTokens inStr)
	let statementList = getStmtList(parseTree)
	putStrLn ("Parse Tree: " ++ show(parseTree))
	putStrLn ("Statement List: " ++ show(statementList))
	let labelStatementList = parseStmtList(statementList)	
	let flowGraph = createGraph labelStatementList empty 1
	putStrLn("Flow Graph: " ++ show(flowGraph))
	print "done"

-- create graph takes a list of LabelStatement and a Graph and an Integer and returns a Graph
createGraph :: [LabelStatement] -> Gr LabelStatement () -> Int -> Gr LabelStatement ()
createGraph (x:xs) g 1 = createGraph xs (([],1,x,[]) & g) 2
createGraph (x:xs) g n = createGraph xs (([((),	(n-1))],n,x,[]) & g) (n+1) 
createGraph [] g n = g


-- takes a StmtList and returns a list of LabelStatements
parseStmtList :: StmtList -> [LabelStatement]
parseStmtList (StmtList stmt stmtList) = parseStmt(stmt):parseStmtList(stmtList)
parseStmtList (NoStmt) = []


parseStmt :: Stmt -> LabelStatement
parseStmt (StmtAssign i a) = SAssign {identifier=i, aexpr=a}


