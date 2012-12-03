.PHONY:buffer
parser: 
	alex scanner.x
	happy parser.y

.PHONY:buffer
buffer:
	ghc -o buffer main_buffer.hs ds.hs ia.hs buffer.hs helper.hs slice.hs dead.hs lv.hs worklist.hs rd.hs reaches.hs ae.hs datatypes.hs flowgraph.hs scanner.hs parser.hs -XScopedTypeVariables

.PHONY:slice
slice:
	ghc -o slice main_slice.hs ds.hs ia.hs buffer.hs helper.hs slice.hs dead.hs lv.hs worklist.hs rd.hs reaches.hs ae.hs datatypes.hs flowgraph.hs scanner.hs parser.hs -XScopedTypeVariables

.PHONY:dead
dead:
	ghc -o dead main_dead.hs ds.hs ia.hs buffer.hs helper.hs slice.hs dead.hs lv.hs worklist.hs rd.hs reaches.hs ae.hs datatypes.hs flowgraph.hs scanner.hs parser.hs -XScopedTypeVariables

.PHONY:signs
signs:
	ghc -o signs main_ds.hs ds.hs ia.hs buffer.hs helper.hs slice.hs dead.hs lv.hs worklist.hs rd.hs reaches.hs ae.hs datatypes.hs flowgraph.hs scanner.hs parser.hs -XScopedTypeVariables

.PHONY:available
available:
	ghc -o available main_aes.hs ds.hs ia.hs buffer.hs helper.hs slice.hs dead.hs lv.hs worklist.hs rd.hs reaches.hs ae.hs datatypes.hs flowgraph.hs scanner.hs parser.hs -XScopedTypeVariables

clean:
	rm -f *.hi
	rm -f *.o
