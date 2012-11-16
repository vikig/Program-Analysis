module RD where

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map
import Parser
import FlowGraph
import Datatypes


getAssignmentLabels :: [(Label, Action)] -> Identifier -> Set (Identifier, Label)
getAssignmentLabels [] _ = Set.empty
getAssignmentLabels list i = u
	where 	firstNode = head list		
		h = rdParseAction firstNode i
		tailNode = tail list		
		t = getAssignmentLabels tailNode i
		u = Set.union h t 

		
			
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
		let	a = i1 ++ "[" ++ show(n) ++ "]"
			u = Set.singleton (a, l) 
		in	u	
	else	
		let	u = Set.empty
		in	u  
rdParseAction (l, (ReadArray i1 (Aexpr1(Aexpr2(Aexpr3 (IntegerLiteral (n))))))) i2 = 
	if 	i1==i2 
	then 	
		let	a = i1 ++ "[" ++ show(n) ++ "]"
			u = Set.singleton (a, l) 
		in	u	
	else	
		let	u = Set.empty
		in	u  
rdParseAction _ _ = u
	where	u = Set.empty




	


killrd :: [(Label, Action)] -> Action -> Label -> Set (Identifier, Label)

killrd vertexList (Assign i _) l = 
	let	h = Set.singleton (i, -1)
 		t = getAssignmentLabels vertexList i
		temp = Set.union h t
		minus = Set.singleton (i,l)
	in	Set.difference temp minus 

killrd vertexList (ArrayAssign i (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral n)))) _) l = 
	let	a = i ++ "[" ++ show(n) ++ "]"
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
	let	a = i ++ "[" ++ show(n) ++ "]"
		h = Set.singleton (a, -1)
 		t = getAssignmentLabels vertexList a
		temp = Set.union h t
		minus = Set.singleton (a,l)
	in	Set.difference temp minus 		

killrd _ _ _ = Set.empty	


genrd :: Action -> Label -> Set (Identifier, Label)
genrd (Assign i _) l = Set.singleton (i, l)
genrd (ReadAct i) l = Set.singleton (i, l)
genrd (ArrayAssign i (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral n)))) _) l = Set.singleton (i ++ "[" ++ show(n) ++ "]", l)
genrd (ReadArray i (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral n)))) ) l = Set.singleton (i ++ "[" ++ show(n) ++ "]", l)
genrd _ _ = Set.empty


fvrd :: [(Label, Action)] -> Set Identifier
fvrd [] = Set.empty
fvrd ((_,(Assign i v)):xs) = u
	where		
		h = Set.singleton i
		t = fvrd xs
		u = Set.union h t
fvrd ((_,(ArrayAssign i (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral n)))) v)):xs) = u
	where		
		a = i ++ "[" ++ show(n) ++ "]" 
		h = Set.singleton a
		t = fvrd xs
		u = Set.union h t
fvrd (_:_) = Set.empty

labelize :: Label -> Identifier -> (Identifier, Label)
labelize l i = (i,l)
{-
entryrd :: FlowGraph -> Label -> Set (Identifier, Label)
entryrd fg l = 
	if l==1
	then 
		let 
			vertexList = labNodes fg			
			fv = fvrd vertexList 		
			finalset = Set.map (labelize (-1)) fv 
		in 	finalset
	else 
		let
			prelist = pre fg l			
			temp = Prelude.map (exitrd fg) prelist
			finalset = Set.unions temp
		in	finalset
-}
extractAction :: Maybe Action -> Action
extractAction (Just a) = a
--should never match this one
extractAction _ = ErrorAct 
	 

exitrd :: FlowGraph -> EntryRD -> Label -> Set (Identifier, Label)
exitrd fg entryset label = result
	where 
		vertexList = labNodes fg		
		action = extractAction (Prelude.lookup label vertexList)		
		killset = killrd vertexList action label
		genset = genrd action label
		tempset = Set.difference entryset killset
		result = Set.union tempset genset 	


rdExtVal :: [(Node, Action)] -> [Analysis]
rdExtVal vertexList = [RDanalysis set]
	where 	fv = fvrd vertexList
		set = Set.map (labelize (-1)) fv
