all : plot 
	@echo "Done all"

plot : exp1 
	@echo "Done plotting"

exp1 : ./Assignment6 
	mkdir -p _data
	# Run 100 times
	for dt in 0.1 0.01 0.001; do \
	    i=1; while [[ $$i -le 100 ]]; do \
		echo Iteration $$i; \
		./Assignment6 100 $$dt _data/data_$${dt}_$$i.txt; \
		((i=i+1)); \
	    done \
	done

./Assignment6 : ./solution.hs
	ghc -O3 $< -hidir /tmp -o Assignment6

