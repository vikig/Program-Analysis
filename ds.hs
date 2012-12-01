module DS where

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Data.Set (Set)
import qualified Data.Set as Set
import Parser
import FlowGraph
import Datatypes
import Helper
import Debug.Trace

cartProd :: [Sign] -> [Sign] -> [(Sign, Sign)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]


calculateAexprSign :: Aexpr -> EntryDS -> Set Sign
calculateAexprSign (Aexpr1 a1) eset = calculateAexprSign1 a1 eset
calculateAexprSign (Plus a a1) eset = Set.unions mapped 
	where 	list1 = Set.toList (calculateAexprSign a eset) 
		list2 = Set.toList (calculateAexprSign1 a1 eset)
		cartesian = cartProd list1 list2
		mapped = map plusSign cartesian
calculateAexprSign (Minus a a1) eset = Set.unions mapped 
	where 	list1 = Set.toList (calculateAexprSign a eset) 
		list2 = Set.toList (calculateAexprSign1 a1 eset)
		cartesian = cartProd list1 list2
		mapped = map minusSign cartesian

calculateAexprSign1 :: Aexpr1 -> EntryDS -> Set Sign
calculateAexprSign1 (Aexpr2 a2) eset = calculateAexprSign2 a2 eset
calculateAexprSign1 (Mul a1 a2) eset = Set.unions mapped 
	where 	list1 = Set.toList (calculateAexprSign1 a1 eset) 
		list2 = Set.toList (calculateAexprSign2 a2 eset)
		cartesian = cartProd list1 list2
		mapped = map mulSign cartesian

calculateAexprSign1 (Div a1 a2) eset = Set.unions mapped 
	where 	list1 = Set.toList (calculateAexprSign1 a1 eset) 
		list2 = Set.toList (calculateAexprSign2 a2 eset)
		cartesian = cartProd list1 list2
		mapped = map divSign cartesian



calculateAexprSign2 :: Aexpr2 -> EntryDS -> Set Sign
calculateAexprSign2 (Aexpr3 a3) eset = calculateAexprSign3 a3 eset
calculateAexprSign2 (Neg a3) eset = Set.unions mapped 
	where 	list1 = [Zero] 
		list2 = Set.toList (calculateAexprSign3 a3 eset)
		cartesian = cartProd list1 list2
		mapped = map minusSign cartesian

calculateAexprSign3 :: Aexpr3 -> EntryDS -> Set Sign
calculateAexprSign3 (Identifier i) eset = calculateIdentifierSign i eset
calculateAexprSign3 (IntegerLiteral n) eset = calculateIntegerSign n 
calculateAexprSign3 (IdentifierArray i (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral n))))) eset = calculateIdentifierSign (i++"["++show(n)++"]") eset
calculateAexprSign3 (IdentifierArray i a) eset = getArraySigns i (Set.toList eset)
calculateAexprSign3 (ABrack a) eset = calculateAexprSign a eset


getArraySigns :: Identifier -> [(Identifier, Set Sign)] -> Set Sign
getArraySigns _ [] = Set.empty
getArraySigns a1 ((a2,set):tail) = Set.union result (getArraySigns a1 tail)
	where
			arrayName = takeWhile (/='[') a2
			result = if a1 == arrayName then set else Set.empty



handleMaybeEmpty :: Maybe (Set Sign) -> Set Sign
handleMaybeEmpty (Just a) = a
--should never match this one
handleMaybeEmpty _ = Set.empty

handleMaybeError :: Maybe (Set Sign) -> Set Sign
handleMaybeError (Just a) = a
--should never match this one
handleMaybeError _ = Set.singleton ErrorSign


calculateIdentifierSign :: Identifier -> EntryDS -> Set Sign
calculateIdentifierSign i entrySet = iset
	where
			list = Set.toList entrySet
			iset = handleMaybeError (lookup i list)
			
calculateIntegerSign :: IntegerLiteral -> Set Sign
calculateIntegerSign n 
	|	n > 0 	= Set.singleton Positive
	|	n < 0	= Set.singleton Negative
	|	n == 0	= Set.singleton Zero

plusSign :: (Sign, Sign) -> Set Sign
plusSign (s1,s2)  
	| s1==Negative 	&& s2==Negative 	= Set.singleton Negative
	| s1==Negative 	&& s2==Zero 		= Set.singleton Negative
	| s1==Negative 	&& s2==Positive 	= Set.union (Set.union (Set.singleton Positive) (Set.singleton Negative)) (Set.singleton Zero)
	| s1==Zero 	&& s2==Negative 	= Set.singleton Negative
	| s1==Zero 	&& s2==Zero 		= Set.singleton Zero
	| s1==Zero 	&& s2==Positive 	= Set.singleton Positive
	| s1==Positive 	&& s2==Negative 	= Set.union (Set.union (Set.singleton Positive) (Set.singleton Negative)) (Set.singleton Zero)
	| s1==Positive 	&& s2==Zero	 	= Set.singleton Positive
	| s1==Positive 	&& s2==Positive 	= Set.singleton Positive

minusSign :: (Sign, Sign) -> Set Sign
minusSign (s1, s2)  
	| s1==Negative 	&& s2==Negative 	= Set.union (Set.union (Set.singleton Positive) (Set.singleton Negative)) (Set.singleton Zero)
	| s1==Negative 	&& s2==Zero 		= Set.singleton Negative
	| s1==Negative 	&& s2==Positive 	= Set.singleton Negative
	| s1==Zero 	&& s2==Negative 	= Set.singleton Positive
	| s1==Zero 	&& s2==Zero 		= Set.singleton Zero
	| s1==Zero 	&& s2==Positive 	= Set.singleton Negative
	| s1==Positive 	&& s2==Negative 	= Set.singleton Positive
	| s1==Positive 	&& s2==Zero	 	= Set.singleton Positive
	| s1==Positive 	&& s2==Positive 	= Set.union (Set.union (Set.singleton Positive) (Set.singleton Negative)) (Set.singleton Zero)

mulSign :: (Sign, Sign) -> Set Sign
mulSign (s1, s2) 
	| s1==Negative 	&& s2==Negative 	= Set.singleton Positive
	| s1==Negative 	&& s2==Zero 		= Set.singleton Zero
	| s1==Negative 	&& s2==Positive 	= Set.singleton Negative
	| s1==Zero 	&& s2==Negative 	= Set.singleton Zero
	| s1==Zero 	&& s2==Zero 		= Set.singleton Zero
	| s1==Zero 	&& s2==Positive 	= Set.singleton Zero
	| s1==Positive 	&& s2==Negative 	= Set.singleton Negative
	| s1==Positive 	&& s2==Zero	 	= Set.singleton Zero
	| s1==Positive 	&& s2==Positive 	= Set.singleton Positive

divSign :: (Sign, Sign) -> Set Sign
divSign (s1, s2)  
	| s1==Negative 	&& s2==Negative 	= Set.singleton Positive
	| s1==Negative 	&& s2==Zero 		= Set.singleton ErrorSign
	| s1==Negative 	&& s2==Positive 	= Set.singleton Negative
	| s1==Zero 	&& s2==Negative 	= Set.singleton Zero
	| s1==Zero 	&& s2==Zero 		= Set.singleton ErrorSign
	| s1==Zero 	&& s2==Positive 	= Set.singleton Zero
	| s1==Positive 	&& s2==Negative 	= Set.singleton Negative
	| s1==Positive 	&& s2==Zero	 	= Set.singleton ErrorSign
	| s1==Positive 	&& s2==Positive 	= Set.singleton Positive




applySign  :: Action -> EntryDS -> Set (Identifier, (Set Sign))
applySign (Assign i a) entrySet = result
	where 
		newSignSet = calculateAexprSign a entrySet
		oldSignSet = handleMaybeError (lookup i (Set.toList entrySet))
		removeOld = Set.difference entrySet (Set.singleton (i,oldSignSet))		
		result = Set.union removeOld (Set.singleton (i, newSignSet))
applySign (ArrayAssign i (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral n)))) a) entrySet = result
	where
		newSignSet = calculateAexprSign a entrySet
		arrayName = (i ++ "[" ++ show(n) ++ "]")
		oldSignSet = handleMaybeError (lookup arrayName (Set.toList entrySet))
		removeOld = Set.difference entrySet (Set.singleton (arrayName, oldSignSet))
		result = Set.union removeOld (Set.singleton (arrayName,newSignSet))

applySign (ReadAct i) entrySet = result
	where
		newSignSet = (Set.union (Set.union (Set.singleton Positive) (Set.singleton Negative)) (Set.singleton Zero))
		oldSignSet = handleMaybeError (lookup i (Set.toList entrySet))
		removeOld = Set.difference entrySet (Set.singleton (i, oldSignSet))		
		result = Set.union removeOld (Set.singleton (i, newSignSet))

applySign (ReadArray i (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral n))))) entrySet = result
	where
		newSignSet = (Set.union (Set.union (Set.singleton Positive) (Set.singleton Negative)) (Set.singleton Zero))
		arrayName = (i ++ "[" ++ show(n) ++ "]")
		oldSignSet = handleMaybeError (lookup arrayName (Set.toList entrySet))
		removeOld = Set.difference entrySet (Set.singleton (arrayName, oldSignSet))		
		result = Set.union removeOld (Set.singleton (arrayName, newSignSet))

applySign _ entrySet = entrySet


unionSign :: [(Identifier, Set Sign)] ->  [(Identifier, Set Sign)] -> Set (Identifier, Set Sign)
unionSign [] _ = Set.empty
unionSign ((i,set):xs) analysisSet = result
	where
			oldSet = handleMaybeEmpty (lookup i analysisSet)
			newSet = Set.union set oldSet
			result = Set.union (Set.singleton (i,newSet)) (unionSign xs analysisSet)



--Exit analysis of a label
exitds :: FlowGraph -> EntryDS -> Label -> Set (Identifier, (Set Sign)) 
exitds fg entryset label = result
	where 
		vertexList = labNodes fg		
		action = extractAction (Prelude.lookup label vertexList)			
		result = applySign action entryset

dsExtVal :: [(Node, Action)] -> [Analysis]
dsExtVal vertexList = [DSanalysis set]
	where 	fv = freevar vertexList
		set = Set.map (makeTuple (Set.empty)) fv
