module Helper where


import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Data.Set (Set)
import qualified Data.Set as Set

import Datatypes



getArrayBound :: DeclList -> Identifier -> Int
getArrayBound NoDecl _ = -1
getArrayBound (DeclList (DeclArray i1 n) dl) i2 = if i1==i2 then (n-1) else (getArrayBound dl i2)
getArrayBound (DeclList _ dl) i = getArrayBound dl i 

-- returns the variables of the program
freevar :: [(Label, Action)] -> Set Identifier
freevar [] = Set.empty
freevar ((_,(Assign i _)):xs) = u
	where		
		h = Set.singleton i
		t = freevar xs
		u = Set.union h t
freevar ((_,(ArrayAssign i (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral n)))) _)):xs) = u
	where		
		a = i ++ "[" ++ Prelude.show(n) ++ "]" 
		h = Set.singleton a
		t = freevar xs
		u = Set.union h t
freevar (_:xs) = freevar xs

makeTuple :: a -> Identifier -> (Identifier, a)
makeTuple l i = (i,l)

-- Extracts an action from a Maybe action, used to deal with the result of the lookup function
extractAction :: Maybe Action -> Action
extractAction (Just a) = a
--should never match this one
extractAction _ = ErrorAct 

getActType :: Action -> ActionType
getActType (Assign _ _) = AssignType
getActType (ArrayAssign _ _ _) = ArrayAssignType
getActType (BooleanAct _) = BooleanActType
getActType (WriteAct _) = WriteActType
getActType (ReadAct _) = ReadActType
getActType (ReadArray _ _) = ReadArrayType
getActType (Skip) = SkipType


-- puts a label and an identifier into a tuple
labelize :: Label -> Identifier -> (Identifier, Label)
labelize l i = (i,l)


--
--
--SHOW FUNCTIONS
--
--

showOverflowAnalysis :: OverflowAnalysis -> String
showOverflowAnalysis [] = []
showOverflowAnalysis ((l,Outside):tail) = "\nAssignment in Label " ++ show(l) ++ " will cause buffer overflow." ++ showOverflowAnalysis tail
showOverflowAnalysis ((l,Partial):tail) = "\nAssignment in Label " ++ show(l) ++ " might cause buffer overflow." ++ showOverflowAnalysis tail
showOverflowAnalysis ((l,Inside):tail) = "\nAssignment in Label " ++ show(l) ++ " is safe." ++ showOverflowAnalysis tail



showVertexList :: VertexList -> String
showVertexList [] = []
showVertexList ((i,a):tail) = show(i) ++ "- << " ++ showAction a ++ " >>\n" ++ showVertexList tail

showAction :: Action -> String
showAction (Assign i a) = i ++ " := " ++ showAexpr(a)
showAction (ArrayAssign i a v) = i ++ "[" ++ showAexpr(a) ++ "] := " ++ showAexpr(v)
showAction (BooleanAct b) = showBexpr b
showAction (WriteAct a) = "write " ++ showAexpr a
showAction (ReadAct i) = "read " ++ i
showAction (ReadArray i a) = "read " ++ i ++"["++ showAexpr a ++"]"  
showAction Skip = "skip"

showAnalysis :: [Analysis] -> Int -> String
showAnalysis [] _ = []
showAnalysis ((AEanalysis a):xs) int = show(int) ++ "- [" ++ showListAexpr (Set.toList a) ++ " ]\n" ++ showAnalysis xs (int+1)
showAnalysis ((RDanalysis a):xs) int = show(int) ++ "- [" ++ showListIdLabel (Set.toList a) ++ " ]\n" ++ showAnalysis xs (int+1)
showAnalysis ((LVanalysis a):xs) int = show(int) ++ "- [" ++ showListId (Set.toList a) ++ " ]\n" ++ showAnalysis xs (int+1)
showAnalysis ((REanalysis a):xs) int = show(int) ++ "- [" ++ showListAexprLabel (Set.toList a) ++ " ]\n" ++ showAnalysis xs (int+1)
showAnalysis ((DSanalysis a):xs) int = show(int) ++ "- [" ++ showListIdSign (Set.toList a) ++ " ]\n" ++ showAnalysis xs (int+1)
showAnalysis ((IAanalysis a):xs) int = show(int) ++ "- [" ++ showListIdInterval (Set.toList a) ++ " ]\n" ++ showAnalysis xs (int+1)

showListAexprLabel :: [(Aexpr,Label)] -> String
showListAexprLabel [] = []
showListAexprLabel ((a,l):tail) = " (" ++ showAexpr(a) ++ ", " ++ show(l) ++ ")" ++ showListAexprLabel tail

showListId :: [Identifier] -> String
showListId [] = []
showListId (i:tail) = " (" ++ i ++ ")" ++ showListId tail

showListIdLabel :: [(Identifier,Label)] -> String
showListIdLabel [] = []
showListIdLabel ((i,(-1)):tail) = " (" ++ i ++ ", ?)" ++ showListIdLabel tail
showListIdLabel ((i,l):tail) = " (" ++ i ++ ", " ++ show(l) ++ ")" ++ showListIdLabel tail

showListIdInterval :: [(Identifier,(Interval, Interval))] -> String
showListIdInterval [] = []
showListIdInterval ((i,(BottomInt,BottomInt)):tail) = " (" ++ i ++ ",BOTTOM)" ++ showListIdInterval tail
showListIdInterval ((i,(ErrorInterval,ErrorInterval)):tail) = " (" ++ i ++ ",ERROR)" ++ showListIdInterval tail
showListIdInterval ((i,(z1,z2)):tail) = " (" ++ i ++ ",[" ++ showInterval(z1)++","++ showInterval(z2) ++ "])" ++ showListIdInterval tail

showInterval :: Interval -> String
showInterval (Z z) = show(z)
showInterval BottomInt = "B"
showInterval InfPos = "+Inf"
showInterval InfNeg= "-Inf"
showInterval ErrorInterval = "ERROR"

showListIdSign :: [(Identifier,(Set Sign))] -> String
showListIdSign [] = []
showListIdSign ((i,set):tail) = " (" ++ i ++ ", {" ++ showSign(Set.toList set) ++ " })" ++ showListIdSign tail

showSign :: [Sign] -> String
showSign [] = []
showSign (Positive:xs) = " +" ++ showSign(xs) 
showSign (Negative:xs) = " -" ++ showSign(xs)
showSign (Zero:xs) = " 0" ++ showSign(xs)
showSign (ErrorSign:xs) = " e" ++ showSign(xs)


showListAexpr :: [Aexpr] -> String
showListAexpr [] = []
showListAexpr (a:tail) = "(" ++ showAexpr a ++ ") " ++ showListAexpr tail

showAexpr :: Aexpr -> String
showAexpr (Aexpr1 a1) = showAexpr1 a1
showAexpr (Plus a a1) = showAexpr a ++ " + " ++ showAexpr1 a1
showAexpr (Minus a a1) = showAexpr a ++ " - " ++ showAexpr1 a1

showAexpr1 :: Aexpr1 -> String
showAexpr1 (Aexpr2 a2) = showAexpr2 a2
showAexpr1 (Mul a1 a2) = showAexpr1 a1 ++ " * " ++ showAexpr2 a2
showAexpr1 (Div a1 a2) = showAexpr1 a1 ++ " / " ++ showAexpr2 a2

showAexpr2 :: Aexpr2 -> String
showAexpr2 (Aexpr3 a3) = showAexpr3 a3
showAexpr2 (Neg a3) = " -" ++ showAexpr3 a3

showAexpr3 :: Aexpr3 -> String
showAexpr3 (Identifier i) = i
showAexpr3 (IntegerLiteral n) = show(n) 
showAexpr3 (IdentifierArray i a) = i++"["++showAexpr(a)++"]"
showAexpr3 (ABrack a) = "(" ++ showAexpr(a) ++ ")"

showBexpr :: Bexpr -> String
showBexpr (Bexpr1 b1) = showBexpr1 b1
showBexpr (Or b b1) = showBexpr b ++ " || " ++ showBexpr1 b1

showBexpr1 (Bexpr2 b2) = showBexpr2 b2
showBexpr1 (And b1 b2) = showBexpr1 b1 ++ " && " ++ showBexpr2 b2

showBexpr2 (GreatThan a1 a2) = showAexpr a1 ++ " > " ++ showAexpr a2
showBexpr2 (LessThan a1 a2) = showAexpr a1 ++ " < " ++ showAexpr a2
showBexpr2 (GreatEqual a1 a2) = showAexpr a1 ++ " >= " ++ showAexpr a2
showBexpr2 (LessEqual a1 a2) = showAexpr a1 ++ " <= " ++ showAexpr a2
showBexpr2 (Equal a1 a2) = showAexpr a1 ++ " = "  ++ showAexpr a2
showBexpr2 (NotEqual a1 a2) = showAexpr a1 ++ " != " ++ showAexpr a2
showBexpr2 (Not b) = "! " ++ showBexpr b
showBexpr2 (Boolean b) = show(b)
showBexpr2 (BBrack b) = "(" ++ showBexpr b ++ ")"

