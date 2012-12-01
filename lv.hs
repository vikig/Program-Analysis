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
genlv :: Action -> Set Identifier
genlv (Assign _ a) = getIdentifiersInSetAexpr [a] 
genlv (ArrayAssign _ a v) = getIdentifiersInSetAexpr (Set.toList (Set.union (Set.singleton a) (Set.singleton v)))
genlv (ReadArray _ a) = getIdentifiersInSetAexpr [a]
genlv (WriteAct a) = getIdentifiersInSetAexpr [a]
genlv (BooleanAct b) = getIdentifiersInSetAexpr (Set.toList (getBexprAexpr b))
genlv _ = Set.empty


	 

--Exit analysis of a label
exitlv :: FlowGraph -> EntryLV -> Label -> Set Identifier
exitlv fg entryset label = result
	where 
		vertexList = labNodes fg		
		action = extractAction (Prelude.lookup label vertexList)		
		killset = killlv action
		genset = genlv action
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


getIdentifiersInSetAexpr :: [Aexpr] -> Set Identifier
getIdentifiersInSetAexpr [] = Set.empty 
getIdentifiersInSetAexpr (head:tail) = Set.union (getIdentifiersInAexpr head) (getIdentifiersInSetAexpr tail)

getIdentifiersInAexpr :: Aexpr -> Set Identifier
getIdentifiersInAexpr (Aexpr1 a1) = getIdentifiersInAexpr1 a1
getIdentifiersInAexpr (Plus a a1) = Set.union (getIdentifiersInAexpr a) (getIdentifiersInAexpr1 a1)
getIdentifiersInAexpr (Minus a a1) = Set.union (getIdentifiersInAexpr a) (getIdentifiersInAexpr1 a1)

getIdentifiersInAexpr1 :: Aexpr1 -> Set Identifier
getIdentifiersInAexpr1 (Aexpr2 a2) = getIdentifiersInAexpr2 a2 
getIdentifiersInAexpr1 (Mul a1 a2) = Set.union (getIdentifiersInAexpr1 a1) (getIdentifiersInAexpr2 a2)
getIdentifiersInAexpr1 (Div a1 a2) = Set.union (getIdentifiersInAexpr1 a1) (getIdentifiersInAexpr2 a2)

getIdentifiersInAexpr2 :: Aexpr2 -> Set Identifier
getIdentifiersInAexpr2 (Aexpr3 a3) = getIdentifiersInAexpr3 a3
getIdentifiersInAexpr2 (Neg a3) = getIdentifiersInAexpr3 a3

getIdentifiersInAexpr3 :: Aexpr3 -> Set Identifier
getIdentifiersInAexpr3 (Identifier i) = Set.singleton i
getIdentifiersInAexpr3 (IdentifierArray i (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral n)))) ) = Set.singleton (show(i) ++ "[" ++ show(n) ++ "]") 
getIdentifiersInAexpr3 (IdentifierArray i a) = Set.union (Set.singleton (i ++ "[-1]")) (getIdentifiersInAexpr a)
getIdentifiersInAexpr3 (IntegerLiteral _) = Set.empty
getIdentifiersInAexpr3 (ABrack a) = getIdentifiersInAexpr a
