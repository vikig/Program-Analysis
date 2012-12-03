module BufferOverflow where

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Data.Set (Set)
import Data.List
import qualified Data.Set as Set
import Debug.Trace

import IA
import Datatypes
import Helper


lessThanZero :: Interval -> Bool
lessThanZero InfNeg = True
lessThanZero InfPos = False
lessThanZero (Z a) 
	| a<0 = True
	| otherwise = False

greaterThanBound :: Interval -> Int -> Bool
greaterThanBound InfNeg _ = False
greaterThanBound InfPos _ = True
greaterThanBound (Z a) b
	| a>b 		= True
	| otherwise 	= False


		

checkIfErrorAnalysis :: [Analysis] -> Bool
checkIfErrorAnalysis [] = False 
checkIfErrorAnalysis ((IAanalysis set):tail) =
	if elem (ErrorInterval,ErrorInterval) intervalList then True else checkIfErrorAnalysis tail
	where
		list = Set.toList set
		intervalList = map snd list


checkRange :: Bool -> Bool -> Bool -> Bool -> RangeCheck
checkRange True True _ _ = Outside
checkRange _ _ True True = Outside
checkRange False _ _ False = Inside
checkRange _ _ _ _ = Partial

bufferOverflow :: DeclList -> [Analysis] -> VertexList -> [(Label,RangeCheck)]
bufferOverflow _ _ [] = []
bufferOverflow dl analysisList ((node,(ArrayAssign a i v)):tail) = [(node,r)] ++ (bufferOverflow dl analysisList tail)

	where
			(IAanalysis analysis) = analysisList!!(node-1)			
			arrayBound = getArrayBound dl a
			(i1,i2) = calculateAexprInterval i analysis
			l1 = lessThanZero i1
			l2 = lessThanZero i2			
			u1 = greaterThanBound i1 arrayBound
			u2 = greaterThanBound i2 arrayBound
			r = checkRange l1 l2 u1 u2

bufferOverflow dl analysisList (notArrayAssign:tail) = bufferOverflow dl analysisList tail  			
