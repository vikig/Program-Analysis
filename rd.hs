module RD where

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map
import Parser
import FlowGraph
import Datatypes
import Helper

--Function that returns the labels where there is an assignment to the identifier
getAssignmentLabels :: [(Label, Action)] -> Identifier -> Set (Identifier, Label)
getAssignmentLabels [] _ = Set.empty
getAssignmentLabels list i = u
	where 	firstNode = head list		
		h = rdParseAction firstNode i
		tailNode = tail list		
		t = getAssignmentLabels tailNode i
		u = Set.union h t 

	
--Parsea an action into a value of the lattice			
rdParseAction :: (Label, Action) -> Identifier -> Set (Identifier, Label)
rdParseAction (l, (Assign i1 _)) i2 = 	
	if 	i1==i2 
	then 	
		let u = Set.singleton (i1, l)
		in u 
	else 	
		let u = Set.empty
		in u

rdParseAction (l, (ReadAct i1)) i2 =
	if 	i1==i2 
	then 	
		let u = Set.singleton (i1, l)
		in u 
	else 	
		let u = Set.empty
		in u
rdParseAction (l, (ArrayAssign i1 (Aexpr1(Aexpr2(Aexpr3 (IntegerLiteral (n))))) _)) i2 = 
	if 	i1==i2 
	then 	
		let	a = i1 ++ "[" ++ Prelude.show(n) ++ "]"
			u = Set.singleton (a, l) 
		in	u	
	else	
		let	u = Set.empty
		in	u  
rdParseAction (l, (ReadArray i1 (Aexpr1(Aexpr2(Aexpr3 (IntegerLiteral (n))))))) i2 = 
	if 	i1==i2 
	then 	
		let	a = i1 ++ "[" ++ Prelude.show(n) ++ "]"
			u = Set.singleton (a, l) 
		in	u	
	else	
		let	u = Set.empty
		in	u  
rdParseAction _ _ = u
	where	u = Set.empty


-- the Kill function
killrd :: [(Label, Action)] -> Action -> Label -> Set (Identifier, Label)

killrd vertexList (Assign i _) l = 
	let	h = Set.singleton (i, -1)
 		t = getAssignmentLabels vertexList i
		temp = Set.union h t
		minus = Set.singleton (i,l)
	in	Set.difference temp minus 

killrd vertexList (ArrayAssign i (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral n)))) _) l = 
	let	a = i ++ "[" ++ Prelude.show(n) ++ "]"
		h = Set.singleton (a, -1)
 		t = getAssignmentLabels vertexList a
		temp = Set.union h t
		minus = Set.singleton (a,l)
	in	Set.difference temp minus 


killrd vertexList (ReadAct i) l = 
	let 	h = Set.singleton (i, -1)
 		t = getAssignmentLabels vertexList i
		temp = Set.union h t
		minus = Set.singleton (i,l)
	in	Set.difference temp minus

killrd vertexList (ReadArray i (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral n))))) l = 
	let	a = i ++ "[" ++ Prelude.show(n) ++ "]"
		h = Set.singleton (a, -1)
 		t = getAssignmentLabels vertexList a
		temp = Set.union h t
		minus = Set.singleton (a,l)
	in	Set.difference temp minus 		

killrd _ _ _ = Set.empty	

-- the gen function
genrd :: Action -> Label -> DeclList -> Set (Identifier, Label)
genrd (Assign i _) l _ = Set.singleton (i, l)
genrd (ReadAct i) l _ = Set.singleton (i, l)
genrd (ArrayAssign i (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral n)))) _) l _ = Set.singleton (i ++ "[" ++ Prelude.show(n) ++ "]", l)
genrd (ArrayAssign i a _) l dl = generateForArray i l arrayBounds
	where
		arrayBounds = getArrayBound dl i 		
genrd (ReadArray i (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral n)))) ) l _ = Set.singleton (i ++ "[" ++ Prelude.show(n) ++ "]", l)
genrd (ReadArray i a) l dl = generateForArray i l arrayBounds
	where
		arrayBounds = getArrayBound dl i
genrd _ _ _= Set.empty

generateForArray :: Identifier -> Label -> Int -> Set (Identifier,Label)
generateForArray _ _ (-1) = Set.empty
generateForArray i l n = Set.union (Set.singleton ((i++"["++show(n)++"]"),l)) (generateForArray i l (n-1)) 


--Exit analysis of a label
exitrd :: FlowGraph -> EntryRD -> Label -> DeclList -> Set (Identifier, Label)
exitrd fg entryset label dl = result
	where 
		vertexList = labNodes fg		
		action = extractAction (Prelude.lookup label vertexList)		
		killset = killrd vertexList action label
		genset = genrd action label dl
		tempset = Set.difference entryset killset
		result = Set.union tempset genset 	

--Function that returns the initial analysis of the program, for every x returns (x,?) where ? is represented by -1 in our program
rdExtVal :: [(Node, Action)] -> [Analysis]
rdExtVal vertexList = [RDanalysis set]
	where 	fv = freevar vertexList
		set = Set.map (makeTuple (-1)) fv
