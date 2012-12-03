module Slice where

import Parser
import Scanner
import FlowGraph
import Datatypes
import RD
import Worklist


import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Data.Set (Set)
import Data.List
import qualified Data.Set as Set


getAexprInAction :: Action -> Set Aexpr
getAexprInAction (Assign i a) = Set.singleton a
getAexprInAction (ArrayAssign a i v) = Set.union (Set.singleton i) (Set.singleton v)
getAexprInAction (BooleanAct b) = getBexprAexpr b
getAexprInAction (WriteAct a) = Set.singleton a
getAexprInAction (ReadAct v) = Set.empty
getAexprInAction (ReadArray a i) = Set.singleton i
getAexprInAction (Skip) = Set.empty

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

getLabelsOfInterest :: [(Identifier,Label)] -> Set Identifier -> Set Label
getLabelsOfInterest [] _ = Set.empty
getLabelsOfInterest ((i,l):tail) set = Set.union headLabel tailLabelSet
	where			
			headLabel = if (elem '[' i) 
					then 	if ((memberHandleArray i (Set.toList set)) && (l /= (-1))) then Set.singleton l else Set.empty
					else	if ((Set.member i set) && (l /= (-1))) then Set.singleton l else Set.empty
			tailLabelSet = getLabelsOfInterest tail set



memberHandleArray :: Identifier -> [Identifier] -> Bool
memberHandleArray _ [] = False
memberHandleArray i (h:t) = answer
	where
		namePart1 = takeWhile (/= '[') i
		namePart2 = takeWhile (/= '[') h
		indexPart1 = takeWhile(/= ']') (tail(dropWhile (/= '[') i))
		indexPart2 = takeWhile(/= ']') (tail(dropWhile (/= '[') h))
		nameEquals = namePart1 == namePart2		
		indexEquals = indexPart1 == indexPart2
		index2isMinusOne = indexPart2 == "-1"		
		answer = if (nameEquals && indexEquals) || (nameEquals && index2isMinusOne)
				then True
				else memberHandleArray i t

		 
mainSlice :: FlowGraph -> [Analysis] -> [Label] -> Set Label -> VertexList
mainSlice _ _ [] _ = []
mainSlice fg analysisList (p:tail) ignoreSet = result

	where
		vertexList = labNodes fg
		headLabels = getSliceLabels vertexList (analysisList!!(p-1)) p
		tail2 = (Set.toList (Set.difference (Set.union headLabels (Set.fromList tail)) ignoreSet))		
		enclosingBoolean1 = getEnclosingBoolean fg p 0 0
		enclosingBoolean2 = getEnclosingBoolean fg p 0 1		
		enclosingBoolean = Data.List.union [enclosingBoolean1] [enclosingBoolean2]		
		unionThisIgnore = Set.union (Set.singleton p) ignoreSet
		booleanVertex = if (enclosingBoolean1 /= (-1)) 
					then 	mainSlice fg analysisList enclosingBoolean (Set.union unionThisIgnore (Set.fromList tail2))
					else	[] 	
		tailVertex = mainSlice fg analysisList tail2 unionThisIgnore
		thisVertex = [vertexList!!(p-1)]
		result = (Data.List.union (Data.List.union thisVertex tailVertex) booleanVertex)
		--result = (Data.List.union thisVertex tailVertex)
		
getSliceLabels :: VertexList -> Analysis -> Label -> Set Label
getSliceLabels vertexList (RDanalysis set) pointOfInterest = labelsOfInterest 
	where
			(_,actionOfInterest) = vertexList!!(pointOfInterest-1)
			setAexprOfInterest = getAexprInAction actionOfInterest
			identifiersOfInterest = {-trace("aexpr of interest " ++ show(setAexprOfInterest))-} (getIdentifiersInSetAexpr (Set.toList setAexprOfInterest))
			labelsOfInterest = {-trace("id of interest " ++ show(identifiersOfInterest))-} getLabelsOfInterest (Set.toList set) identifiersOfInterest
