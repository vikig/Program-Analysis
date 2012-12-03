module LV where

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Data.Set (Set)
import qualified Data.Set as Set
import Parser
import FlowGraph
import Datatypes
import Helper


-- the Kill function
killlv :: Action -> Set Identifier

killlv (Assign i _) = Set.singleton i 
killlv (ArrayAssign i (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral n)))) _) = Set.singleton (i ++ "[" ++ show(n) ++ "]")
killlv (ReadAct i) = Set.singleton i
killlv (ReadArray i (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral n))))) = Set.singleton (i ++ "[" ++ show(n) ++ "]")
killlv _ = Set.empty	

-- the gen function
genlv :: Action -> DeclList -> Set Identifier
genlv (Assign _ a) dl = getIdentifiersInSetAexpr [a] dl
genlv (ArrayAssign _ a v) dl = getIdentifiersInSetAexpr (Set.toList (Set.union (Set.singleton a) (Set.singleton v))) dl
genlv (ReadArray _ a) dl = getIdentifiersInSetAexpr [a] dl
genlv (WriteAct a) dl = getIdentifiersInSetAexpr [a] dl
genlv (BooleanAct b) dl = getIdentifiersInSetAexpr (Set.toList (getBexprAexpr b)) dl
genlv _ _ = Set.empty


	 

--Exit analysis of a label
exitlv :: FlowGraph -> EntryLV -> Label -> DeclList -> Set Identifier
exitlv fg entryset label dl = result
	where 
		vertexList = labNodes fg		
		action = extractAction (Prelude.lookup label vertexList)		
		killset = killlv action
		genset = genlv action dl
		tempset = Set.difference entryset killset
		result = Set.union tempset genset 	

--Function that returns the initial analysis of the program, for every x returns (x,?) where ? is represented by -1 in our program
lvExtVal :: [(Node, Action)] -> [Analysis]
lvExtVal vertexList = [LVanalysis (freevar vertexList)]
		

getBexprAexpr :: Bexpr -> Set Aexpr
getBexprAexpr (Bexpr1 b1) = getBexpr1Aexpr b1
getBexprAexpr (Or b b1) = Set.union (getBexprAexpr b) (getBexpr1Aexpr b1)

getBexpr1Aexpr :: Bexpr1 -> Set Aexpr
getBexpr1Aexpr (Bexpr2 b2) = getBexpr2Aexpr b2
getBexpr1Aexpr (And b1 b2) = Set.union (getBexpr1Aexpr b1) (getBexpr2Aexpr b2)

getBexpr2Aexpr :: Bexpr2 -> Set Aexpr
getBexpr2Aexpr (GreatThan a1 a2) = Set.union (Set.singleton a1) (Set.singleton a2)
getBexpr2Aexpr (LessThan a1 a2) = Set.union (Set.singleton a1) (Set.singleton a2)
getBexpr2Aexpr (GreatEqual a1 a2) = Set.union (Set.singleton a1) (Set.singleton a2)
getBexpr2Aexpr (LessEqual a1 a2) = Set.union (Set.singleton a1) (Set.singleton a2)
getBexpr2Aexpr (Equal a1 a2) = Set.union (Set.singleton a1) (Set.singleton a2)
getBexpr2Aexpr (NotEqual a1 a2) = Set.union (Set.singleton a1) (Set.singleton a2)
getBexpr2Aexpr (Not b) = getBexprAexpr b
getBexpr2Aexpr (BBrack b) = getBexprAexpr b
getBexpr2Aexpr (Boolean _) = Set.empty


getIdentifiersInSetAexpr :: [Aexpr] -> DeclList -> Set Identifier
getIdentifiersInSetAexpr [] _ = Set.empty 
getIdentifiersInSetAexpr (head:tail) dl = Set.union (getIdentifiersInAexpr head dl) (getIdentifiersInSetAexpr tail dl)

getIdentifiersInAexpr :: Aexpr -> DeclList -> Set Identifier
getIdentifiersInAexpr (Aexpr1 a1) dl = getIdentifiersInAexpr1 a1 dl
getIdentifiersInAexpr (Plus a a1) dl = Set.union (getIdentifiersInAexpr a dl) (getIdentifiersInAexpr1 a1 dl)
getIdentifiersInAexpr (Minus a a1) dl = Set.union (getIdentifiersInAexpr a dl) (getIdentifiersInAexpr1 a1 dl)

getIdentifiersInAexpr1 :: Aexpr1 -> DeclList -> Set Identifier
getIdentifiersInAexpr1 (Aexpr2 a2) dl = getIdentifiersInAexpr2 a2 dl 
getIdentifiersInAexpr1 (Mul a1 a2) dl = Set.union (getIdentifiersInAexpr1 a1 dl) (getIdentifiersInAexpr2 a2 dl)
getIdentifiersInAexpr1 (Div a1 a2) dl = Set.union (getIdentifiersInAexpr1 a1 dl) (getIdentifiersInAexpr2 a2 dl)

getIdentifiersInAexpr2 :: Aexpr2 -> DeclList -> Set Identifier
getIdentifiersInAexpr2 (Aexpr3 a3) dl = getIdentifiersInAexpr3 a3 dl
getIdentifiersInAexpr2 (Neg a3) dl = getIdentifiersInAexpr3 a3 dl

getIdentifiersInAexpr3 :: Aexpr3 -> DeclList -> Set Identifier
getIdentifiersInAexpr3 (Identifier i) _ = Set.singleton i
getIdentifiersInAexpr3 (IdentifierArray i (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral n)))) ) _ = Set.singleton (i ++ "[" ++ show(n) ++ "]") 
getIdentifiersInAexpr3 (IdentifierArray i a) dl = Set.union a1 a2
	where
		arrayBound = getArrayBound dl i		
		a1 = generateForArray i arrayBound
		a2 = getIdentifiersInAexpr a dl
getIdentifiersInAexpr3 (IntegerLiteral _) _ = Set.empty
getIdentifiersInAexpr3 (ABrack a) dl = getIdentifiersInAexpr a dl



generateForArray :: Identifier -> Int -> Set Identifier
generateForArray i (-1) = Set.empty
generateForArray i n = Set.union (Set.singleton this) rest
	where
		
		this = i ++ "[" ++ show(n) ++ "]"
		rest = generateForArray i (n-1)
