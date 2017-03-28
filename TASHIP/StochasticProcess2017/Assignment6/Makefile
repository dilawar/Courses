all : plot 
	echo "Done all"

plot : run 
	echo "Done plotting"


run : ./solution.hs 
	ghc -O3 $< -hidir /tmp -o Assignment6
	./Assignment6 100 0.1 data.txt

