module IA where

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Data.Set (Set)
import qualified Data.Set as Set
import Parser
import FlowGraph
import Datatypes
import Helper
import Debug.Trace

minInt = -100
maxInt = 100

cartProd :: [Interval] -> [Interval] -> [(Interval, Interval)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]


calculateAexprInterval :: Aexpr -> EntryIA -> (Interval,Interval)
calculateAexprInterval (Aexpr1 a1) eset = calculateAexprInterval1 a1 eset
calculateAexprInterval (Plus a a1) eset = plusInterval int1 int2 
	where 	int1 = calculateAexprInterval a eset 
		int2 = calculateAexprInterval1 a1 eset
		
calculateAexprInterval (Minus a a1) eset = minusInterval int1 int2 
	where 	int1 = calculateAexprInterval a eset 
		int2 = calculateAexprInterval1 a1 eset

calculateAexprInterval1 :: Aexpr1 -> EntryIA -> (Interval, Interval)
calculateAexprInterval1 (Aexpr2 a2) eset = calculateAexprInterval2 a2 eset
calculateAexprInterval1 (Mul a1 a2) eset = mulInterval int1 int2 
	where 	int1 = calculateAexprInterval1 a1 eset 
		int2 = calculateAexprInterval2 a2 eset
calculateAexprInterval1 (Div a1 a2) eset = divInterval int1 int2 
	where 	int1 = calculateAexprInterval1 a1 eset 
		int2 = calculateAexprInterval2 a2 eset



calculateAexprInterval2 :: Aexpr2 -> EntryIA -> (Interval, Interval)
calculateAexprInterval2 (Aexpr3 a3) eset = calculateAexprInterval3 a3 eset
calculateAexprInterval2 (Neg a3) eset = minusInterval int1 int2 
	where 	int1 = (Z 0, Z 0) 
		int2 = calculateAexprInterval3 a3 eset
		
calculateAexprInterval3 :: Aexpr3 -> EntryIA -> (Interval, Interval)
calculateAexprInterval3 (Identifier i) eset = calculateIdentifierInterval i eset
calculateAexprInterval3 (IntegerLiteral n) eset = calculateIntegerInterval n 
calculateAexprInterval3 (IdentifierArray i (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral n))))) eset = calculateIdentifierInterval (i++"["++show(n)++"]") eset
--TODO calculateAexprInterval3 (IdentifierArray i a) = 
calculateAexprInterval3 (ABrack a) eset = calculateAexprInterval a eset


handleMaybeError :: Maybe (Interval, Interval) -> (Interval, Interval)
handleMaybeError (Just a) = a
--should never match this one
handleMaybeError _ = (ErrorInterval,ErrorInterval)




calculateIdentifierInterval :: Identifier -> EntryIA -> (Interval, Interval)
calculateIdentifierInterval i entrySet = interval
	where
			list = Set.toList entrySet
			interval = handleMaybeError (lookup i list)
			
calculateIntegerInterval :: IntegerLiteral -> (Interval, Interval)
calculateIntegerInterval n 
	|	n > maxInt			= (InfPos,InfPos)
	|	n < minInt 			= (InfNeg,InfNeg)
	|	otherwise			= (Z n, Z n)

plusInterval :: (Interval, Interval) -> (Interval, Interval) -> (Interval, Interval)
plusInterval (InfNeg,_) (_,InfPos)	= (InfNeg,InfPos)
plusInterval (InfNeg,_) (_,_)		= (InfNeg,InfNeg)
plusInterval (_,InfNeg) (_,_) 		= (InfNeg,InfNeg)
plusInterval (_,_) (InfNeg,_) 		= (InfNeg,InfNeg)
plusInterval (_,_) (_,InfNeg) 		= (InfNeg,InfNeg)
plusInterval (_,InfPos) (InfNeg,_)	= (InfNeg,InfPos)
plusInterval (InfPos,_) (_,_)		= (InfPos,InfPos)
plusInterval (_,InfPos) (_,_) 		= (InfPos,InfPos)
plusInterval (_,_) (InfPos,_) 		= (InfPos,InfPos)
plusInterval (_,_) (_,InfPos) 		= (InfPos,InfPos)
plusInterval (BottomInt,_) (_,_)	= (BottomInt,BottomInt)
plusInterval (_,BottomInt) (_,_) 	= (BottomInt,BottomInt)
plusInterval (_,_) (BottomInt,_) 	= (BottomInt,BottomInt)
plusInterval (_,_) (_,BottomInt) 	= (BottomInt,BottomInt)
plusInterval (Z z11,Z z12) (Z z21,Z z22) = (z1,z2) 
	where
		sum1 = z11+z21
		sum2 = z12+z22
		z1 = if (sum1 < minInt) then InfNeg else if (sum1 > maxInt) then InfPos else (Z sum1)
		z2 = if (sum2 < minInt) then InfNeg else if (sum2 > maxInt) then InfPos else (Z sum2)	

		
minusInterval :: (Interval, Interval) -> (Interval, Interval) -> (Interval, Interval)
minusInterval (InfNeg,_) (InfNeg,_)	= (InfNeg,InfPos)
minusInterval (InfNeg,_) (_,_)		= (InfNeg,InfNeg)
minusInterval (_,InfNeg) (_,_) 		= (InfNeg,InfNeg)
minusInterval (_,_) (InfNeg,_) 		= (InfPos,InfPos)
minusInterval (_,_) (_,InfNeg) 		= (InfPos,InfPos)
minusInterval (_,InfPos) (_,InfPos)	= (InfNeg,InfPos)
minusInterval (InfPos,_) (_,_)		= (InfPos,InfPos)
minusInterval (_,InfPos) (_,_) 		= (InfPos,InfPos)
minusInterval (_,_) (InfPos,_) 		= (InfNeg,InfNeg)
minusInterval (_,_) (_,InfPos) 		= (InfNeg,InfNeg)
minusInterval (BottomInt,_) (_,_)	= (BottomInt,BottomInt)
minusInterval (_,BottomInt) (_,_) 	= (BottomInt,BottomInt)
minusInterval (_,_) (BottomInt,_) 	= (BottomInt,BottomInt)
minusInterval (_,_) (_,BottomInt) 	= (BottomInt,BottomInt)
minusInterval (Z z11,Z z12) (Z z21,Z z22) = (z1,z2) 
	where
		sum1 = z11-z22
		sum2 = z12-z21
		z1 = if (sum1 < minInt) then InfNeg else if (sum1 > maxInt) then InfPos else (Z sum1)
		z2 = if (sum2 < minInt) then InfNeg else if (sum2 > maxInt) then InfPos else (Z sum2)

mulInterval :: (Interval, Interval) -> (Interval, Interval) -> (Interval, Interval)
mulInterval (InfNeg,_) (_,_)		= (InfNeg,InfNeg)
mulInterval (_,InfNeg) (_,_) 		= (InfNeg,InfNeg)
mulInterval (_,_) (InfNeg,_) 		= (InfNeg,InfNeg)
mulInterval (_,_) (_,InfNeg) 		= (InfNeg,InfNeg)
mulInterval (InfPos,_) (_,_)		= (InfPos,InfPos)
mulInterval (_,InfPos) (_,_) 		= (InfPos,InfPos)
mulInterval (_,_) (InfPos,_) 		= (InfPos,InfPos)
mulInterval (_,_) (_,InfPos) 		= (InfPos,InfPos)
mulInterval (BottomInt,_) (_,_)	= (BottomInt,BottomInt)
mulInterval (_,BottomInt) (_,_) 	= (BottomInt,BottomInt)
mulInterval (_,_) (BottomInt,_) 	= (BottomInt,BottomInt)
mulInterval (_,_) (_,BottomInt) 	= (BottomInt,BottomInt)
mulInterval (Z a,Z b) (Z c,Z d) = (z1,z2) 
	where
		cartesian = [a*c,a*d,b*c,b*d]		
		mul1 = minimum cartesian
		mul2 = maximum cartesian
		z1 = if (mul1 < minInt) 
			then InfNeg 
			else 
				if (mul1 > maxInt) 
					then InfPos 
					else (Z mul1)
		z2 = if (mul2 < minInt) 
			then InfNeg 
			else 
				if (mul2 > maxInt) 
					then InfPos 
					else (Z mul2)

divInterval :: (Interval, Interval) -> (Interval, Interval) -> (Interval, Interval)
divInterval (InfNeg,_) (_,_)		= (InfNeg,InfNeg)
divInterval (_,InfNeg) (_,_) 		= (InfNeg,InfNeg)
divInterval (_,_) (InfNeg,_) 		= (InfNeg,InfNeg)
divInterval (_,_) (_,InfNeg) 		= (InfNeg,InfNeg)
divInterval (InfPos,_) (_,_)		= (InfPos,InfPos)
divInterval (_,InfPos) (_,_) 		= (InfPos,InfPos)
divInterval (_,_) (InfPos,_) 		= (InfPos,InfPos)
divInterval (_,_) (_,InfPos) 		= (InfPos,InfPos)
divInterval (BottomInt,_) (_,_)		= (BottomInt,BottomInt)
divInterval (_,BottomInt) (_,_) 	= (BottomInt,BottomInt)
divInterval (_,_) (BottomInt,_) 	= (BottomInt,BottomInt)
divInterval (_,_) (_,BottomInt) 	= (BottomInt,BottomInt)
divInterval (Z a,Z b) (Z c,Z d) = (z1,z2) 
	where
		cartesian = [div a c, div a d, div b c, div b d]		
		div1 = minimum cartesian
		div2 = maximum cartesian
		z1 = if (False == (c<=0&&d>=0)) 
			then 
				if (div1 < minInt) 
					then InfNeg 
					else 
						if (div1 > maxInt) 
							then InfPos 
							else (Z div1)
			else 	ErrorInterval
		z2 = if (False == (c<=0&&d>=0))
			then 
				if (div2 < minInt) 
					then InfNeg 
					else 
						if (div2 > maxInt) 
							then InfPos 
							else (Z div2)
			else	ErrorInterval

applyInterval  :: Action -> EntryIA -> Set (Identifier, (Interval,Interval))
applyInterval (Assign i a) entrySet = result
	where 
		newIntervalTuple = calculateAexprInterval a entrySet
		oldIntervalTuple = handleMaybeError (lookup i (Set.toList entrySet))
		removeOld = Set.difference entrySet (Set.singleton (i,oldIntervalTuple))		
		result = Set.union removeOld (Set.singleton (i, newIntervalTuple))
applyInterval (ArrayAssign i (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral n)))) a) entrySet = result
	where
		newIntervalTuple = calculateAexprInterval a entrySet
		arrayName = (i ++ "[" ++ show(n) ++ "]")
		oldIntervalTuple = handleMaybeError (lookup arrayName (Set.toList entrySet))
		removeOld = Set.difference entrySet (Set.singleton (arrayName, oldIntervalTuple))
		result = Set.union removeOld (Set.singleton (arrayName,newIntervalTuple))

applyInterval (ReadAct i) entrySet = result
	where
		newIntervalTuple = (Z minInt, Z maxInt)
		oldIntervalTuple = handleMaybeError (lookup i (Set.toList entrySet))
		removeOld = Set.difference entrySet (Set.singleton (i, oldIntervalTuple))		
		result = Set.union removeOld (Set.singleton (i, newIntervalTuple))

applyInterval (ReadArray i (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral n))))) entrySet = result
	where
		newIntervalTuple = (Z minInt, Z maxInt)
		arrayName = (i ++ "[" ++ show(n) ++ "]")
		oldIntervalTuple = handleMaybeError (lookup arrayName (Set.toList entrySet))
		removeOld = Set.difference entrySet (Set.singleton (arrayName, oldIntervalTuple))		
		result = Set.union removeOld (Set.singleton (arrayName, newIntervalTuple))

applyInterval _ entrySet = entrySet


unionInterval :: [(Identifier, (Interval,Interval))] ->  [(Identifier, (Interval,Interval))] -> Set (Identifier, (Interval,Interval))
unionInterval [] _ = Set.empty
unionInterval ((i,newSet):xs) analysisSet = result
	where
			oldSet = handleMaybeError (lookup i analysisSet)
			result = Set.union (Set.singleton (i,newSet)) (unionInterval xs analysisSet)





--Exit analysis of a label
exitia :: FlowGraph -> EntryIA -> Label -> Set (Identifier, ((Interval, Interval))) 
exitia fg entryset label = result
	where 
		vertexList = labNodes fg		
		action = extractAction (Prelude.lookup label vertexList)			
		result = applyInterval action entryset

iaExtVal :: [(Node, Action)] -> [Analysis]
iaExtVal vertexList = [IAanalysis set]
	where 	fv = freevar vertexList
		set = Set.map (makeTuple ((BottomInt,BottomInt))) fv
