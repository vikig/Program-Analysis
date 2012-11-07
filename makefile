default: 

	rm -f scanner.hs
	rm -f parser.hs
	alex scanner.x
	happy parser.y
	ghc -o main main.hs scanner.hs parser.hs
	

clean:
	rm -f *.hs
	rm -f *.hi
	rm -f *.o
	rm -f main
	
