all : exp1 docs
	@echo "Done all"

docs : ./Assignment6.tex 
	pdflatex --shell-escape $<
	@echo "Done plotting"

exp1 : Assignment6 
	mkdir -p _data
	for v in 0 10; do \
	    for dt in 0.1 0.01 0.001; do \
		i=1; while [[ $$i -le 100 ]]; do \
		    echo Iteration $$i; \
		    ./Assignment6 300 $$dt $$v _data/$${v}_$${dt}_$${i}.txt; \
		    ((i=i+1)); \
		done; \
		tail -n 1 -q _data/$${v}_* > $${v}_$${dt}_lastVel.txt;  \
	    done; \
	done;
	# Generate file with velocity

Assignment6 : solution.hs
	ghc -O3 $< -hidir /tmp -o Assignment6

