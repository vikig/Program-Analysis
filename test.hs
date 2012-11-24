module Test where

import Parser
import Scanner
import FlowGraph
import Datatypes
import RD
import AE
import Worklist
import Reaches


import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Data.Set (Set)
import qualified Data.Set as Set

--test rd.hs
{-
testGetAssignmentLabels :: IO()
testGetAssignmentLabels = do
	let a1 = Assign "x" (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral 1))))
	let a2 = Assign "y" (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral 1))))
	let a3 = Assign "x" (Aexpr1(Aexpr2(Aexpr3(Identifier "y"))))
	let list = [(1,a1),(2,a2),(3,a3)]
	let set = getAssignmentLabels list "x"
	putStrLn("Result: " ++ show(set))

	print "done"

testRDParseAction :: IO()
testRDParseAction = do
	let assignment = Assign "x" (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral 1))))
	let read = ReadAct "x"
	let skip = Skip	
	let notx = ReadAct "y"
	let input1 = (1,assignment)
	let input2 = (1,read)
	let input3 = (1,skip)
	let input4 = (1,notx)	
	let set1 = rdParseAction input1 "x"
	let set2 = rdParseAction input2 "x"
	let set3 = rdParseAction input3 "x"
	let set4 = rdParseAction input4 "x"
	
	putStrLn("Assign Action: " ++ show(set1))
	putStrLn("Read Action: " ++ show(set2))
	putStrLn("Skip Action: " ++ show(set3))
	putStrLn("Not x Action: " ++ show(set4))

	print "done" 

testKillRD :: IO()
testKillRD = do
	let a1 = Assign "x" (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral 1))))
	let a2 = Assign "y" (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral 1))))
	let a3 = Assign "x" (Aexpr1(Aexpr2(Aexpr3(Identifier "y"))))
	let a4 = Skip	
	let list = [(1,a1),(2,a2),(3,a3),(4,a4)]
	let set = killrd list a3 3
	putStrLn("Result: " ++ show(set))

	print "done"

testfvrd :: IO()
testfvrd = do
	let a1 = Assign "x" (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral 1))))
	let a2 = Assign "y" (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral 1))))
	let a3 = Assign "z" (Aexpr1(Aexpr2(Aexpr3(Identifier "y"))))
	let a4 = Skip	
	let list = [(1,a1),(2,a2),(3,a3),(4,a4)]
	let set = fvrd list
	putStrLn("Result: " ++ show(set))

	print "done"
{-
testentryrd :: IO()
testentryrd = do
	let a1 = Assign "x" (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral 1))))
	let a2 = Assign "y" (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral 1))))
	let a3 = Assign "z" (Aexpr1(Aexpr2(Aexpr3(Identifier "y"))))
	let a4 = Skip	
	let vlist = [(1,a1),(2,a2),(3,a3),(4,a4)]
	let elist = [(1,2,()),(2,3,()),(2,3,()),(3,4,())]
	let fg = mkGraph vlist elist
	let set = entryrd fg 1
	putStrLn("Result: " ++ show(set))

	print "done"
-}
testexitrd :: IO()
testexitrd = do
	let a1 = Assign "x" (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral 1))))
	let a2 = Assign "y" (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral 1))))
	let a3 = Assign "z" (Aexpr1(Aexpr2(Aexpr3(Identifier "y"))))
	let a4 = Skip	
	let vlist = [(1,a1),(2,a2),(3,a3),(4,a4)]
	let elist = [(1,2,()),(2,3,()),(2,3,()),(3,4,())]
	let fg = mkGraph vlist elist
	let entryset = Set.fromList [("x",1),("y",2),("z",(-1))]
	let set = exitrd fg entryset 3
	putStrLn("Result: " ++ show(set))

	print "done"

testWorklistInit :: IO()
testWorklistInit = do
	let a1 = Assign "x" (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral 1))))
	let a2 = Assign "y" (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral 1))))
	let a3 = Assign "z" (Aexpr1(Aexpr2(Aexpr3(Identifier "y"))))
	let a4 = Skip	
	let vlist = [(1,a1),(2,a2),(3,a3),(4,a4)]
	let extval = RDExtVal
	let bottom = RDanalysis (Set.empty)
	let analysis = worklistInit vlist [1] extval bottom
	putStrLn("Result: " ++ show(analysis))

	print "done"

testApplyTransFunct :: IO()
testApplyTransFunct = do
	let a1 = Assign "x" (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral 1))))
	let a2 = Assign "y" (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral 1))))
	let a3 = Assign "z" (Aexpr1(Aexpr2(Aexpr3(Identifier "y"))))
	let a4 = Skip	
	let vlist = [(1,a1),(2,a2),(3,a3),(4,a4)]
	let elist = [(1,2,()),(2,3,()),(2,3,()),(3,4,())]
	let fg = mkGraph vlist elist
	let trans = [(AssignType,RDFunction),(SkipType,NoOp)]
	let entryset = Set.fromList [("x",1),("y",(-1)),("z",(-1))]
	let analysis = applyTransFunct 2 (RDanalysis entryset) trans fg
	putStrLn("Result: " ++ show(analysis))

	print "done"

testReplaceNth :: IO()
testReplaceNth = do
	let a1 = Assign "x" (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral 1))))
	let a2 = Assign "y" (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral 1))))
	let a3 = Assign "z" (Aexpr1(Aexpr2(Aexpr3(Identifier "y"))))
	let a4 = Skip	
	let vlist = [(1,a1),(2,a2),(3,a3),(4,a4)]
	let elist = [(1,2,()),(2,3,()),(2,3,()),(3,4,())]
	let fg = mkGraph vlist elist
	let trans = [(AssignType,RDFunction),(SkipType,NoOp)]
	let entryset = Set.fromList [("x",1),("y",(-1)),("z",(-1))]
	let analysis = applyTransFunct 2 (RDanalysis entryset) trans fg
	putStrLn("Result: " ++ show([analysis]))
	let replaced = replaceNth 1 RDExtVal [analysis]
	putStrLn("Replaced: " ++ show(replaced))	

testCompareAnalysis :: IO()
testCompareAnalysis = do
	let a = RDanalysis (Set.fromList [("x",1)])
	let b = RDanalysis (Set.fromList [("x",1),("y",2)])
	let answer = compareAnalysis a b
	putStrLn("A = " ++ show(a))
	putStrLn("B = " ++ show(b))
	putStrLn("is A not a subset of B = " ++ show(answer))
	let c = RDanalysis (Set.fromList [("z",3)])
	let d = RDanalysis (Set.fromList [("x",1),("y",2)])
	let answer2 = compareAnalysis c d
	putStrLn("C = " ++ show(c))
	putStrLn("D = " ++ show(d))
	putStrLn("is C not a subset of D = " ++ show(answer2))


testWorklistWork :: IO()
testWorklistWork = do
	let a1 = Assign "x" (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral 1))))
	let a2 = Assign "y" (Aexpr1(Aexpr2(Aexpr3(IntegerLiteral 2))))
	let a3 = Assign "z" (Aexpr1(Aexpr2(Aexpr3(Identifier "y"))))
	let a4 = Skip	
	let vlist = [(1,a1),(2,a2),(3,a3),(4,a4)]
	let elist = [(1,2,()),(2,3,()),(2,3,()),(3,4,())]
	let trans = [(AssignType,RDFunction),(SkipType,NoOp)]
	let fg = mkGraph vlist elist 
	let extval = RDExtVal
	let bottom = RDanalysis (Set.empty)
	let ianalysis = worklistInit vlist [1] extval bottom
	putStrLn("Initial analysis: " ++ show(ianalysis)) 
	let fanalysis = worklistWork elist ianalysis trans fg
	putStrLn("Final analysis: " ++ show(fanalysis))
	print "done"

-}

aexprr = (Aexpr1(Aexpr2(Aexpr3(Identifier "a"))))
aexpr11 = (Aexpr2(Aexpr3(IntegerLiteral 1)))
aexpr12 = (Aexpr2(Aexpr3(Identifier "b")))	
aexpr13 = (Aexpr2(Aexpr3(Identifier "a")))	
action = Assign "a" (Plus aexprr aexpr11)
aexpr2 = (Aexpr3(Identifier "x"))
entryList = [((Aexpr1(Mul aexpr13 aexpr2)),2),((Plus aexprr aexpr11),1),((Plus aexprr aexpr11),5)]  
aexprrr = Plus aexprr aexpr11	


testExitOneAexpr :: IO()
testExitOneAexpr = do
	let result = exitOneAexpr action aexprrr 4 entryList
	putStrLn("result: " ++ show(result))
	print "done"

testGetExpressionsThatContain :: IO()
testGetExpressionsThatContain = do
	let result = getExpressionsThatContain [(Plus aexprr aexpr11)] "a"
	putStrLn("result: " ++ show(result))
	print "done"





