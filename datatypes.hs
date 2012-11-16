module Datatypes where

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Data.Set (Set)
import qualified Data.Set as Set

type FlowGraph = Gr Action ()
type VertexList = [(Node, Action)]

type Label = Int

type Flow = [UEdge]
type ExtLab = [Node]
type ExtVal = Analysis
type Bottom = Analysis
type Worklist = [UEdge]

type EntryRD = Set (Identifier, Label)



data Framework = MonFramework [Function] FlowGraph ExtLab ExtVal TransFunct 

data Function = 
	RDFunction
	|
	NoOp
	|
	ErrorFunct
	deriving(Show, Eq)
type TransFunct = (ActionType, Function)


data Analysis =
	RDExtVal
	|	
	RDanalysis (Set (Identifier, Label))
	|
	ErrorAnalysis
	deriving(Show, Eq)

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
	|
	ErrorAct
	deriving(Show, Eq)



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
	deriving(Show, Eq)

data Aexpr1
	= Aexpr2 Aexpr2
	| Mul Aexpr1 Aexpr2
	| Div Aexpr1 Aexpr2
	deriving(Show, Eq)

data Aexpr2
	= Neg Aexpr3
	| Aexpr3 Aexpr3
	deriving(Show, Eq)

data Aexpr3
	= Identifier Identifier
	| IdentifierArray Identifier Aexpr
	| IntegerLiteral IntegerLiteral
	| ABrack Aexpr
	deriving(Show, Eq)

data Bexpr 
	= Bexpr1 Bexpr1
	| Or Bexpr Bexpr1
	deriving(Show, Eq)

data Bexpr1
	= Bexpr2 Bexpr2
	| And Bexpr1 Bexpr2
	deriving(Show, Eq)

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
	deriving(Show, Eq)

type Identifier	= String
type IntegerLiteral = Int
type Boolean = Bool







