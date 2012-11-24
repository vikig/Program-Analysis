module AE where

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map
import Data.List
import Parser
import FlowGraph
import Datatypes


-- gets expressions that contain the identifier
getExpressionsThatContain :: [Aexpr] -> Identifier -> Set Aexpr
getExpressionsThatContain [] _ = Set.empty
getExpressionsThatContain (x:xs) i = u
	where	h = if (checkAexpr x i) then Set.singleton x else Set.empty
		t = getExpressionsThatContain xs i
		u = Set.union h t
		
-- checks if the identifier is part of the Aexpr
checkAexpr :: Aexpr -> Identifier -> Bool
checkAexpr (Aexpr1 a1) i = checkAexpr1 a1 i
checkAexpr (Plus a a1) i = (checkAexpr a i) || (checkAexpr1 a1 i)
checkAexpr (Minus a a1) i = (checkAexpr a i) ||  (checkAexpr1 a1 i)

checkAexpr1 :: Aexpr1 -> Identifier -> Bool
checkAexpr1 (Aexpr2 a2) i = checkAexpr2 a2 i
checkAexpr1 (Mul a1 a2) i = (checkAexpr1 a1 i) ||  (checkAexpr2 a2 i)
checkAexpr1 (Div a1 a2) i = (checkAexpr1 a1 i) ||  (checkAexpr2 a2 i)

checkAexpr2 :: Aexpr2 -> Identifier -> Bool
checkAexpr2 (Aexpr3 a3) i = checkAexpr3 a3 i
checkAexpr2 (Neg a3) i = checkAexpr3 a3 i

checkAexpr3 :: Aexpr3 -> Identifier -> Bool
checkAexpr3 (Identifier i1) i = if (i1==i) then True else False
checkAexpr3 (IdentifierArray i1 a) i = answer 
	where 	indexPart = takeWhile(/= ']') (tail(dropWhile (/= '[') i))	
		answer = if ((isSuffixOf i1 i) && (checkAexpr a indexPart)) 
				then True 
				else False
checkAexpr3 (IntegerLiteral _) _ = False
checkAexpr3 (ABrack a) i = checkAexpr a i

-- the Kill function
killae :: [(Label, Action)] -> Action -> Set Aexpr -> Set Aexpr

killae vertexList (Assign i a) entryset = 
	let	aexpset = aexp vertexList
		temp = getExpressionsThatContain (Set.toList aexpset) i
		minus = Set.singleton a
	in	Set.difference temp minus 

killae vertexList (ArrayAssign i (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral n)))) a) entryset = 
	let	i' = i ++ "[" ++ Prelude.show(n) ++ "]"
		aexpset = aexp vertexList
		temp = getExpressionsThatContain (Set.toList aexpset) i
 		minus = Set.singleton a
	in	Set.difference temp minus 
 		
killae _ _ _ = Set.empty	

-- the gen function
genae :: Action -> Set Aexpr
genae (Assign i a) = if (checkAexpr a i) then Set.empty else Set.singleton a 
{-
may not be true
-}
genae (ArrayAssign i (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral n)))) a) = 
	if (checkAexpr a (i ++ "[" ++ Prelude.show(n) ++ "]")) 
		then Set.empty
		else Set.singleton a

genae (BooleanAct b) = getBexprAexpr b
genae _ = Set.empty


checkNonTrivial0 :: Aexpr -> Bool
checkNonTrivial0 (Aexpr1 a1) = checkNonTrivial1 a1
checkNonTrivial0 _ = True

checkNonTrivial1 :: Aexpr1 -> Bool
checkNonTrivial1 (Aexpr2 a2) = checkNonTrivial2 a2
checkNonTrivial1 _ = True

checkNonTrivial2 :: Aexpr2 -> Bool
checkNonTrivial2 (Neg a3) = checkNonTrivial3 a3    
checkNonTrivial2 (Aexpr3 a3) = checkNonTrivial3 a3

checkNonTrivial3 :: Aexpr3 -> Bool
checkNonTrivial3 (ABrack a) = checkNonTrivial0 a
checkNonTrivial3 _ = False

getAexpr :: Aexpr -> Set Aexpr
getAexpr a = if (checkNonTrivial0 a) then Set.singleton a else Set.empty


getBexprAexpr :: Bexpr -> Set Aexpr
getBexprAexpr (Bexpr1 b1) = getBexpr1Aexpr b1
getBexprAexpr (Or b b1) = Set.union (getBexprAexpr b) (getBexpr1Aexpr b1)

getBexpr1Aexpr :: Bexpr1 -> Set Aexpr
getBexpr1Aexpr (Bexpr2 b2) = getBexpr2Aexpr b2
getBexpr1Aexpr (And b1 b2) = Set.union (getBexpr1Aexpr b1) (getBexpr2Aexpr b2)

getBexpr2Aexpr :: Bexpr2 -> Set Aexpr
getBexpr2Aexpr (GreatThan a1 a2) = Set.union (getAexpr a1) (getAexpr a2)
getBexpr2Aexpr (LessThan a1 a2) = Set.union (getAexpr a1) (getAexpr a2)
getBexpr2Aexpr (GreatEqual a1 a2) = Set.union (getAexpr a1) (getAexpr a2)
getBexpr2Aexpr (LessEqual a1 a2) = Set.union (getAexpr a1) (getAexpr a2)
getBexpr2Aexpr (Equal a1 a2) = Set.union (getAexpr a1) (getAexpr a2)
getBexpr2Aexpr (NotEqual a1 a2) = Set.union (getAexpr a1) (getAexpr a2)
getBexpr2Aexpr (Not b) = getBexprAexpr b
getBexpr2Aexpr (BBrack b) = getBexprAexpr b
getBexpr2Aexpr (Boolean _) = Set.empty


-- returns the expressions of the program
aexp :: [(Label, Action)] -> Set Aexpr
aexp [] = Set.empty
aexp ((_,(Assign _ value)):xs) = u
	where		
		h = getAexpr value
		t = aexp xs
		u = Set.union h t
aexp ((_,(ArrayAssign _ _ value)):xs) = u
	where		
		h = getAexpr value
		t = aexp xs
		u = Set.union h t
aexp ((_,(BooleanAct b)):xs) = u
	where
		h = getBexprAexpr b
		t = aexp xs
		u = Set.union h t

aexp (_:_) = Set.empty

-- puts a label and an identifier into a tuple
labelize :: Label -> Identifier -> (Identifier, Label)
labelize l i = (i,l)

-- Extracts an action from a Maybe action, used to deal with the result of the lookup function
extractAction :: Maybe Action -> Action
extractAction (Just a) = a
--should never match this one
extractAction _ = ErrorAct 
	 

--Exit analysis of a label
exitae :: FlowGraph -> EntryAE -> Label -> Set Aexpr
exitae fg entryset label = result
	where 
		vertexList = labNodes fg		
		action = extractAction (Prelude.lookup label vertexList)		
		killset = killae vertexList action entryset
		genset = genae action
		tempset = Set.difference entryset killset
		result = Set.union tempset genset
