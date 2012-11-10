import Data.Set (Set)
import qualified Data.Set as Set


getAssignmentLabels :: [(Label, Action)] -> Identifier -> Set (Identifier, Label)
getAssignmentLabels ((l,a):xs) i = u
	where 	h = rdParseAction (l,a) i
		t = getAssignmentLabels xs i
		u = union h t 
		
			
rdParseAction :: (Label, Action) -> Identifier -> Set (Identifier, Label)
rdParseAction (l, (Assign i1 _)) i2 = 	
	if 	i1=i2 
	then 	
		let u = singleton (i1, l)
		in u 
	else 	
		let u :: Set (Identifier, Label) = empty
		in u

rdParseAction (l, (ReadAct i1)) i2 =
	if 	i1=i2 
	then 	
		let u = singleton (i1, l)
		in u 
	else 	
		let u :: Set (Identifier, Label) = empty
		in u
rdParseAction _ _ = u
	where	u :: Set (Identifier, Label) = empty

{-
rdParseAction (l, (ArrayAssign i1 (Aexpr1(Aexpr2(Aexpr3 (IntegerLiteral (a))))) _)) i2 = 
	if 	i1=i2 
	then 	[(i1, l)] 
	else []   
-}	


killrd :: [(Label, Action)] -> Action -> Label -> Set (Identifier, Label)

killrd vertexList (Assign i _) l = 
	let 	s :: Set (Identifier, Label) = empty
		h = insert (i, -1) s
 		t = getAssignmentLabels vertexList i
	in	union h t

killrd vertexList (ReadAct i) l = 
		let 	s :: Set (Identifier, Label) = empty
		h = insert (i, -1) s
 		t = getAssignmentLabels vertexList i
	in	union h t
		
killrd _ _ _ = s
	where s :: Set (Identifier, Label) = empty	
	

genrd :: Action -> Label -> [(Identifier, Label)]
genrd (Assign i _) l = s
	where	h :: Set (Identifier, Label) = empty
		s = insert (i, l) h
genrd (ReadAct i) l = s
	where	h :: Set (Identifier, Label) = empty
		s = insert (i, l) h
genrd _ _ = s
	where s :: Set (Identifier, Label) = empty


-- assume fg (flow graph is global)
-- check the labelize part to see if is valid
entryrd :: Label -> Set (Identifier, Label)
entryrd l = 
	if l=1
	then 
		let 
			vertexList = labNodes fg			
			fv = fvrd vertexList 		
			finalset = map labelize (fv -> -1) 
		in 	finalset
	else
		let
			prelist = pre fg l
			preset = fromList prelist			
			temp = map exitrd preset
			finalset = unions temp
		in	finalset


labelize :: Identifier -> Label -> (Identifier, Label)
labelize i l = (i,l)

exitrd :: Label -> Set (Identifier, Label)
exitrd l = result
	where 
		entryset = entryrd l
		killset = killarray!l
		genset = genarray!l
		tempset = difference entryset killset
		result = union tempset genset 			
		
		

fvrd :: [(Label, Action)] -> Set Identifier
fvrd ((Assign i v):xs) = u
	where		
		h = singleton i
		t = fvrd xs
		u = union h t





