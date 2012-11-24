module Slice where

import Parser
import Scanner
import FlowGraph
import Datatypes
import RD
import Worklist
import AE

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
getIdentifiersInAexpr3 (IdentifierArray i a) = Set.union (Set.singleton (show(i) ++ "[-1]")) (getIdentifiersInAexpr a)
getIdentifiersInAexpr3 (IntegerLiteral _) = Set.empty
getIdentifiersInAexpr3 (ABrack a) = getIdentifiersInAexpr a

getLabelsOfInterest :: [(Identifier,Label)] -> Set Identifier -> Set Label
getLabelsOfInterest [] _ = Set.empty
getLabelsOfInterest ((i,l):tail) set = Set.union headLabel tailLabelSet
	where
			headLabel = if ((Set.member i set) && (l /= (-1))) then Set.singleton l else Set.empty
			tailLabelSet = getLabelsOfInterest tail set



		 
mainSlice :: VertexList -> [Analysis] -> [Label] -> Set Label -> VertexList
mainSlice _ _ [] _ = []
mainSlice vertexList analysisList (p:tail) ignoreSet = Data.List.union thisVertex tailVertex
	where
		headLabels = getSliceLabels vertexList (analysisList!!(p-1)) p
		tail2 = Set.toList (Set.difference (Set.union headLabels (Set.fromList tail)) ignoreSet)		
		tailVertex = mainSlice vertexList analysisList tail2 (Set.union (Set.singleton p) ignoreSet)
		thisVertex = [vertexList!!(p-1)]
		


getSliceLabels :: VertexList -> Analysis -> Label -> Set Label
getSliceLabels vertexList (RDanalysis set) pointOfInterest = labelsOfInterest 
	where
			(_,actionOfInterest) = vertexList!!(pointOfInterest-1)
			setAexprOfInterest = getAexprInAction actionOfInterest
			identifiersOfInterest = getIdentifiersInSetAexpr (Set.toList setAexprOfInterest)
			labelsOfInterest = getLabelsOfInterest (Set.toList set) identifiersOfInterest


