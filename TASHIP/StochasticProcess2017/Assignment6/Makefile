all : plot 
	@echo "Done all"

plot : exp1 
	@echo "Done plotting"


exp1 : ./Assignment6 
	mkdir -p _data
	./Assignment6 100 0.1 _data/data.txt

./Assignment6 : ./solution.hs
	ghc -O3 $< -hidir /tmp -o Assignment6

