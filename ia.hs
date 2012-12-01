module IA where

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Data.Set (Set)
import qualified Data.Set as Set
import Parser
import FlowGraph
import Datatypes
import Helper


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
calculateAexprInterval3 (IdentifierArray i a) eset = (z1,z2)
	where
		arrayIntervals = getArrayIntervals i (Set.toList eset)
		lowerBounds = map fst arrayIntervals
		upperBounds = map snd arrayIntervals
		z1 = minimumInterval lowerBounds
		z2 = maximumInterval upperBounds
 
calculateAexprInterval3 (ABrack a) eset = calculateAexprInterval a eset


getArrayIntervals :: Identifier -> [(Identifier, (Interval,Interval))] -> [(Interval,Interval)]
getArrayIntervals _ [] = []
getArrayIntervals a1 ((a2,interTuple):tail) = result ++ (getArrayIntervals a1 tail)
	where
			arrayName = takeWhile (/='[') a2
			result = if a1 == arrayName then [interTuple] else []



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
plusInterval (BottomInt,_) (_,_)	= (BottomInt,BottomInt)
plusInterval (_,BottomInt) (_,_) 	= (BottomInt,BottomInt)
plusInterval (_,_) (BottomInt,_) 	= (BottomInt,BottomInt)
plusInterval (_,_) (_,BottomInt) 	= (BottomInt,BottomInt)
plusInterval (InfNeg,_) (_,InfPos)	= (InfNeg,InfPos)
plusInterval (_,InfPos) (InfNeg,_)	= (InfNeg,InfPos)
plusInterval (_,_) (InfPos,_) 		= (InfPos,InfPos)
plusInterval (_,_) (_,InfPos) 		= (InfPos,InfPos)
plusInterval (InfNeg,_) (_,_)		= (InfNeg,InfNeg)
plusInterval (_,InfPos) (_,_) 		= (InfPos,InfPos)
plusInterval (InfPos,_) (_,_)		= (InfPos,InfPos)
plusInterval (_,InfNeg) (_,_) 		= (InfNeg,InfNeg)
plusInterval (_,_) (InfNeg,_) 		= (InfNeg,InfNeg)
plusInterval (_,_) (_,InfNeg) 		= (InfNeg,InfNeg)
plusInterval (Z z11,Z z12) (Z z21,Z z22) = (z1,z2) 
	where
		sum1 = z11+z21
		sum2 = z12+z22
		z1 = if (sum1 < minInt) then InfNeg else if (sum1 > maxInt) then InfPos else (Z sum1)
		z2 = if (sum2 < minInt) then InfNeg else if (sum2 > maxInt) then InfPos else (Z sum2)	

		
minusInterval :: (Interval, Interval) -> (Interval, Interval) -> (Interval, Interval)
minusInterval (BottomInt,_) (_,_)	= (BottomInt,BottomInt)
minusInterval (_,BottomInt) (_,_) 	= (BottomInt,BottomInt)
minusInterval (_,_) (BottomInt,_) 	= (BottomInt,BottomInt)
minusInterval (_,_) (_,BottomInt) 	= (BottomInt,BottomInt)
minusInterval (InfNeg,_) (InfNeg,_)	= (InfNeg,InfPos)
minusInterval (_,InfPos) (_,InfPos)	= (InfNeg,InfPos)
minusInterval (_,InfNeg) (_,_) 		= (InfNeg,InfNeg)
minusInterval (_,_) (InfNeg,_) 		= (InfPos,InfPos)
minusInterval (_,_) (_,InfNeg) 		= (InfPos,InfPos)
minusInterval (InfPos,_) (_,_)		= (InfPos,InfPos)
minusInterval (_,InfPos) (_,_) 		= (InfPos,InfPos)
minusInterval (_,_) (InfPos,_) 		= (InfNeg,InfNeg)
minusInterval (_,_) (_,InfPos) 		= (InfNeg,InfNeg)

minusInterval (InfNeg,_) (_,_)		= (InfNeg,InfNeg)
minusInterval (Z z11,Z z12) (Z z21,Z z22) = (z1,z2) 
	where
		sum1 = z11-z22
		sum2 = z12-z21
		z1 = if (sum1 < minInt) then InfNeg else if (sum1 > maxInt) then InfPos else (Z sum1)
		z2 = if (sum2 < minInt) then InfNeg else if (sum2 > maxInt) then InfPos else (Z sum2)


multiplyInterval :: Interval -> Interval -> Interval
multiplyInterval BottomInt _ = BottomInt
multiplyInterval _ BottomInt = BottomInt
multiplyInterval ErrorInterval _ = ErrorInterval
multiplyInterval _ ErrorInterval = ErrorInterval
multiplyInterval InfNeg InfNeg = InfPos
multiplyInterval InfNeg InfPos = InfNeg
multiplyInterval InfPos InfNeg = InfNeg
multiplyInterval InfPos InfPos = InfPos
multiplyInterval (Z 0) _ = (Z 0)
multiplyInterval _ (Z 0) = (Z 0)
multiplyInterval InfNeg (Z a)  
			| a<0 	= InfPos
			| a>0 	= InfNeg
multiplyInterval (Z a) InfNeg  
			| a<0 	= InfPos
			| a>0 	= InfNeg
multiplyInterval (Z a) InfPos 
			| a<0   = InfNeg
			| a>0 	= InfPos
multiplyInterval InfPos (Z a) 
			| a<0 	= InfNeg
			| a>0 	= InfPos
multiplyInterval (Z a) (Z b) = (Z (a*b))

divideInterval :: Interval -> Interval -> Interval
divideInterval BottomInt _ = BottomInt
divideInterval _ BottomInt = BottomInt
divideInterval ErrorInterval _ = ErrorInterval
divideInterval _ ErrorInterval = ErrorInterval
divideInterval InfNeg InfNeg = InfPos
divideInterval InfNeg InfPos = InfNeg
divideInterval InfPos InfNeg = InfNeg
divideInterval InfPos InfPos = InfPos
divideInterval (Z 0) _ = (Z 0)
divideInterval _ (Z 0) = ErrorInterval
divideInterval InfNeg (Z a)  
			| a<0 	= InfPos
			| a>0 	= InfNeg
divideInterval (Z a) InfNeg = (Z 0)  
divideInterval (Z a) InfPos = (Z 0)

divideInterval InfPos (Z a) 
			| a<0 	= InfNeg
			| a>0 	= InfPos

divideInterval (Z a) (Z b) = (Z (div a b))


convertToNumber :: [Interval] -> [Int]
convertToNumber [] = []
convertToNumber ((Z a):tail) = [a] ++ convertToNumber tail
convertToNumber (InfPos:tail) = convertToNumber tail
convertToNumber (InfNeg:tail) = convertToNumber tail
convertToNumber (ErrorInterval:tail) = convertToNumber tail
convertToNumber (BottomInt:tail) = convertToNumber tail

minimumInterval :: [Interval] -> Interval
minimumInterval list = 
	if (elem ErrorInterval list) 
		then ErrorInterval
		else if (elem BottomInt list) 
			then BottomInt	
			else if (elem InfNeg list) 
				then InfNeg
				else if (Set.fromList list) == (Set.singleton InfPos)
					then	InfPos
					else 	(Z (minimum (convertToNumber list))) 
maximumInterval :: [Interval] -> Interval
maximumInterval list = 
	if (elem ErrorInterval list) 
		then ErrorInterval
		else if (elem BottomInt list) 
			then BottomInt	
			else if (elem InfPos list) 
				then InfPos
				else if (Set.fromList list) == (Set.singleton InfNeg)
					then 	InfNeg
					else	(Z (maximum (convertToNumber list))) 

extractNumber :: Interval -> Int
extractNumber (Z a) = a

evaluateMinMax :: Interval -> Interval
evaluateMinMax ErrorInterval = ErrorInterval
evaluateMinMax BottomInt = BottomInt
evaluateMinMax InfNeg = InfNeg
evaluateMinMax InfPos = InfPos
evaluateMinMax (Z a) 
		| a<minInt 	= InfNeg
		| a>maxInt 	= InfPos
		| otherwise 	= (Z a)

			

mulInterval :: (Interval, Interval) -> (Interval, Interval) -> (Interval, Interval)
mulInterval (a,b) (c,d) = (z1,z2) 
	where
		cartesian = [(multiplyInterval a c),(multiplyInterval a d),(multiplyInterval b c),(multiplyInterval b d)]		
		mul1 = minimum cartesian
		mul2 = maximum cartesian
		z1 = evaluateMinMax mul1
		z2 = evaluateMinMax mul2
	

divInterval :: (Interval, Interval) -> (Interval, Interval) -> (Interval, Interval)
divInterval (a,b) (c,d) = (z1,z2) 
	where
		cartesian = [(divideInterval a c),(divideInterval a d),(divideInterval b c),(divideInterval b d)]		
		mul1 = minimum cartesian
		mul2 = maximum cartesian
		z1 = evaluateMinMax mul1
		z2 = evaluateMinMax mul2

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
