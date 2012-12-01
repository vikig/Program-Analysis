parser: 
	alex scanner.x
	happy parser.y
buffer:
	ghc -o buffer main_buffer.hs ds.hs ia.hs buffer.hs helper.hs slice.hs dead.hs lv.hs worklist.hs rd.hs reaches.hs ae.hs datatypes.hs flowgraph.hs scanner.hs parser.hs -XScopedTypeVariables

slice:
	ghc -o slice main_slice.hs ds.hs ia.hs buffer.hs helper.hs slice.hs dead.hs lv.hs worklist.hs rd.hs reaches.hs ae.hs datatypes.hs flowgraph.hs scanner.hs parser.hs -XScopedTypeVariables

dead:
	ghc -o dead main_dead.hs ds.hs ia.hs buffer.hs helper.hs slice.hs dead.hs lv.hs worklist.hs rd.hs reaches.hs ae.hs datatypes.hs flowgraph.hs scanner.hs parser.hs -XScopedTypeVariables

signs:
	ghc -o signs main_ds.hs ds.hs ia.hs buffer.hs helper.hs slice.hs dead.hs lv.hs worklist.hs rd.hs reaches.hs ae.hs datatypes.hs flowgraph.hs scanner.hs parser.hs -XScopedTypeVariables

available:
	ghc -o available main_aes.hs ds.hs ia.hs buffer.hs helper.hs slice.hs dead.hs lv.hs worklist.hs rd.hs reaches.hs ae.hs datatypes.hs flowgraph.hs scanner.hs parser.hs -XScopedTypeVariables

clean:
	rm -f scanner.hs	
	rm -f parser.hs
	rm -f *.hi
	rm -f *.o
	rm -f buffer
	rm -f slice
	rm -f dead
	rm -f signs
	
