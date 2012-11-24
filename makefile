default: 

	#rm -f scanner.hs
	#rm -f parser.hs
	#alex scanner.x
	#happy parser.y
	ghc -o main main.hs slice.hs worklist.hs rd.hs reaches.hs ae.hs datatypes.hs flowgraph.hs scanner.hs parser.hs -XScopedTypeVariables

test:

	ghci test.hs worklist.hs rd.hs reaches.hs ae.hs datatypes.hs flowgraph.hs scanner.hs parser.hs -XScopedTypeVariables

clean:
	rm -f *.hs
	rm -f *.hi
	rm -f *.o
	rm -f main
	
