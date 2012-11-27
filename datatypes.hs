-- this file contains all the different datatypes we use for the analyzer 

module Datatypes where

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Data.Set (Set)
import qualified Data.Set as Set

-- a FlowGraph is a Graph with Action vertex and unlabeled edges
type FlowGraph = Gr Action ()
-- list of vertices of the flow graph, which consist of tuples of Node (integer) and Action
type VertexList = [(Node, Action)]

-- Flow graph label
type Label = Int

-- Monotone framework flow (list of edges of the flowgraph)
type Flow = [UEdge]
-- Extremal Labels of the flowgraph (list of nodes)
type ExtLab = [Node]
-- Extremal value of the flowgraph
type ExtVal = Analysis
type Bottom = Analysis
-- Worklist, list of edges
-- Bottom value of the lattice
type Worklist = [UEdge]

-- Set of lattice values used by Reaching definitions to calculate the Exit set
type EntryRD = Set (Identifier, Label)
type EntryAE = Set Aexpr
type EntryReaches = Set (Aexpr, (Set Label))
type EntryLV = Set Identifier
type EntryDS = Set (Identifier, Set Sign)


 
data Sign =
	Negative
	|
	Positive
	|
	Zero
	|
	ErrorSign
	deriving(Show,Eq)


--Functions that transfer functions map to
data Function =
	LVFunction	
	|	
	REFunction	
	|
	AEFunction
	| 
	RDFunction
	|
	NoOp
	|
	-- if an ActionType is not mapped by any transfer function then it is mapped to an errorfunction
	ErrorFunct
	deriving(Show, Eq)

--Mapping from ActionType to Functions
type TransFunct = (ActionType, Function)

--Possible values of Analysis[x]
data Analysis =	
	AEInitVal	
	|	
	AEExtVal
	|	
	RDExtVal
	|
	LVExtVal
	|
	REExtVal	
	|
	RDanalysis (Set (Identifier,Label))
	|
	AEanalysis (Set Aexpr)
	|
	REanalysis (Set (Aexpr,Label))
	|
	LVanalysis (Set Identifier)
	|
	DSanalysis (Set (Identifier,(Set Sign)))
	|
	ErrorAnalysis
	deriving(Show, Eq)

showVertexList :: VertexList -> String
showVertexList [] = []
showVertexList ((i,a):tail) = show(i) ++ "- << " ++ showAction a ++ " >>\n" ++ showVertexList tail

showAction :: Action -> String
showAction (Assign i a) = i ++ " := " ++ showAexpr(a)
showAction (ArrayAssign i a v) = i ++ "[" ++ showAexpr(a) ++ "] := " ++ showAexpr(v)
showAction (BooleanAct b) = showBexpr b
showAction (WriteAct a) = "write " ++ showAexpr a
showAction (ReadAct i) = "read " ++ i
showAction (ReadArray i a) = "read " ++ i ++"["++ showAexpr a ++"]"  
showAction Skip = "skip"

showAnalysis :: [Analysis] -> Int -> String
showAnalysis [] _ = []
showAnalysis ((AEanalysis a):xs) int = show(int) ++ "- [" ++ showListAexpr (Set.toList a) ++ "]\n" ++ showAnalysis xs (int+1)
showAnalysis ((RDanalysis a):xs) int = show(int) ++ "- [" ++ showListIdLabel (Set.toList a) ++ "]\n" ++ showAnalysis xs (int+1)
showAnalysis ((LVanalysis a):xs) int = show(int) ++ "- [" ++ showListId (Set.toList a) ++ "]\n" ++ showAnalysis xs (int+1)
showAnalysis ((REanalysis a):xs) int = show(int) ++ "- [" ++ showListAexprLabel (Set.toList a) ++ "]\n" ++ showAnalysis xs (int+1)

showListAexprLabel :: [(Aexpr,Label)] -> String
showListAexprLabel [] = []
showListAexprLabel ((a,l):tail) = "(" ++ showAexpr(a) ++ ", " ++ show(l) ++ ")" ++ showListAexprLabel tail

showListId :: [Identifier] -> String
showListId [] = []
showListId (i:tail) = "(" ++ i ++ ")," ++ showListId tail

showListIdLabel :: [(Identifier,Label)] -> String
showListIdLabel [] = []
showListIdLabel ((i,l):tail) = "(" ++ i ++ ", " ++ show(l) ++ ")," ++ showListIdLabel tail


showListAexpr :: [Aexpr] -> String
showListAexpr [] = []
showListAexpr (a:tail) = "(" ++ showAexpr a ++ ") " ++ showListAexpr tail

showAexpr :: Aexpr -> String
showAexpr (Aexpr1 a1) = showAexpr1 a1
showAexpr (Plus a a1) = showAexpr a ++ " + " ++ showAexpr1 a1
showAexpr (Minus a a1) = showAexpr a ++ " - " ++ showAexpr1 a1

showAexpr1 :: Aexpr1 -> String
showAexpr1 (Aexpr2 a2) = showAexpr2 a2
showAexpr1 (Mul a1 a2) = showAexpr1 a1 ++ " * " ++ showAexpr2 a2
showAexpr1 (Div a1 a2) = showAexpr1 a1 ++ " / " ++ showAexpr2 a2

showAexpr2 :: Aexpr2 -> String
showAexpr2 (Aexpr3 a3) = showAexpr3 a3
showAexpr2 (Neg a3) = " -" ++ showAexpr3 a3

showAexpr3 :: Aexpr3 -> String
showAexpr3 (Identifier i) = i
showAexpr3 (IntegerLiteral n) = show(n) 
showAexpr3 (IdentifierArray i a) = i++"["++showAexpr(a)++"]"
showAexpr3 (ABrack a) = "(" ++ showAexpr(a) ++ ")"

showBexpr :: Bexpr -> String
showBexpr (Bexpr1 b1) = showBexpr1 b1
showBexpr (Or b b1) = showBexpr b ++ " || " ++ showBexpr1 b1

showBexpr1 (Bexpr2 b2) = showBexpr2 b2
showBexpr1 (And b1 b2) = showBexpr1 b1 ++ " && " ++ showBexpr2 b2

showBexpr2 (GreatThan a1 a2) = showAexpr a1 ++ " > " ++ showAexpr a2
showBexpr2 (LessThan a1 a2) = showAexpr a1 ++ " < " ++ showAexpr a2
showBexpr2 (GreatEqual a1 a2) = showAexpr a1 ++ " >= " ++ showAexpr a2
showBexpr2 (LessEqual a1 a2) = showAexpr a1 ++ " <= " ++ showAexpr a2
showBexpr2 (Equal a1 a2) = showAexpr a1 ++ " = "  ++ showAexpr a2
showBexpr2 (NotEqual a1 a2) = showAexpr a1 ++ " != " ++ showAexpr a2
showBexpr2 (Not b) = "! " ++ showBexpr b
showBexpr2 (Boolean b) = show(b)
showBexpr2 (BBrack b) = "(" ++ showBexpr b ++ ")"


--Type of different actions
data ActionType =
	AssignType
	|
	ArrayAssignType
	|
	BooleanActType
	|
	WriteActType
	|	
	ReadActType
	|
	ReadArrayType
	|
	SkipType
	deriving(Show, Eq)

getActType :: Action -> ActionType
getActType (Assign _ _) = AssignType
getActType (ArrayAssign _ _ _) = ArrayAssignType
getActType (BooleanAct _) = BooleanActType
getActType (WriteAct _) = WriteActType
getActType (ReadAct _) = ReadActType
getActType (ReadArray _ _) = ReadArrayType
getActType (Skip) = SkipType

--Abstraction of Program statements to be put into the nodes of the flowgraph
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
	aexpr		:: Aexpr
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
	|
	ErrorAct
	deriving(Show, Eq)



--
--
-- Datatypes used by the parser
--
--
data Program
	= Program DeclBody StmtList
	deriving(Show, Eq)

 

data DeclBody
	= DeclBody DeclList
	| EmptyDeclBody
	deriving(Show, Eq)

data DeclList
	= DeclList Decl DeclList
	| NoDecl
	deriving(Show, Eq)

data Decl
	= Decl Identifier
	| DeclArray Identifier IntegerLiteral
	deriving(Show, Eq)

data StmtList
	= StmtList Stmt StmtList
	| NoStmt
	deriving(Show, Eq)

data Stmt
	= StmtAssign Identifier Aexpr
	| StmtAssignArray Identifier Aexpr Aexpr
	| StmtSkip
	| StmtIf Bexpr StmtList StmtList
	| StmtRead Identifier
	| StmtReadArray Identifier Aexpr
	| StmtWrite Aexpr
	| StmtWhile Bexpr StmtList
	deriving(Show, Eq)

data Aexpr
	= Aexpr1 Aexpr1
	| Plus Aexpr Aexpr1
	| Minus Aexpr Aexpr1
	deriving(Show, Eq, Ord)

data Aexpr1
	= Aexpr2 Aexpr2
	| Mul Aexpr1 Aexpr2
	| Div Aexpr1 Aexpr2
	deriving(Show, Eq, Ord)

data Aexpr2
	= Neg Aexpr3
	| Aexpr3 Aexpr3
	deriving(Show, Eq, Ord)

data Aexpr3
	= Identifier Identifier
	| IdentifierArray Identifier Aexpr
	| IntegerLiteral IntegerLiteral
	| ABrack Aexpr
	deriving(Show, Eq, Ord)

data Bexpr 
	= Bexpr1 Bexpr1
	| Or Bexpr Bexpr1
	deriving(Show, Eq, Ord)

data Bexpr1
	= Bexpr2 Bexpr2
	| And Bexpr1 Bexpr2
	deriving(Show, Eq, Ord)

data Bexpr2
	= GreatThan Aexpr Aexpr
	| LessThan Aexpr Aexpr
	| GreatEqual Aexpr Aexpr
	| LessEqual Aexpr Aexpr
	| Equal Aexpr Aexpr
	| NotEqual Aexpr Aexpr
	| Not Bexpr
	| Boolean Boolean
	| BBrack Bexpr
	deriving(Show, Eq, Ord)

type Identifier	= String
type IntegerLiteral = Int
type Boolean = Bool







