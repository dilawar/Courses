all : solve 
	time ./solve +RTS -K1000M

solve : solve.hs
	ghc -O3 -rtsopts $< -o $@

clean : 
	rm -rf *.hi *.ho *.o solve
