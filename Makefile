all:index

index:
	ghc input.hs

clean: 
	rm *.hi *.o 
