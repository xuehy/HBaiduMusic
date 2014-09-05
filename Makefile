main : Main.hs MainGUI.hs 
	ghc -O3 -threaded -rtsopts Main.hs -package bytestring-0.9.2.1 -package network-2.3.0.13
	
.PHONY : clean
clean :
	rm *.hi *.o
