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
type EntryLV = Set Identifier
type EntryDS = Set (Identifier, Set Sign)
type EntryIA = Set (Identifier, (Interval,Interval))
	
-- Used in Buffer overflow analysis to mark if a range is partially inside, outside or completely inside another range
data RangeCheck = 
	Partial
	| 
	Outside
	|
	Inside
	deriving(Show,Eq)
 

-- Detection of Signs 
data Sign =
	Negative
	|
	Positive
	|
	Zero
	|
	ErrorSign -- Division by Zero
	deriving(Show,Eq,Ord)

-- Interval Analysis
data Interval =
	Z Int -- number
	|
	BottomInt -- bottom element
	|
	InfNeg -- infinite negative
	|
	InfPos -- infinite positive
	|
	ErrorInterval -- division by zero
	deriving(Show,Eq,Ord)
	 


--Functions that transfer functions map to
data Function =
	IAFunction
	|
	DSFunction	
	|
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
	IAExtVal	
	|
	IAInitVal	
	|
	DSInitVal	
	|
	DSExtVal
	|
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
	IAanalysis (Set (Identifier,(Interval,Interval)))
	|
	ErrorAnalysis
	deriving(Show, Eq)

type OverflowAnalysis = [(Label,RangeCheck)]

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
