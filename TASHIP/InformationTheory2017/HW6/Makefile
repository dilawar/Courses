all : ./hw61.py.png ./HW6.pdf ./markov1.dot.pdf rook.dot.pdf king.dot.pdf bishop.dot.pdf
	@echo "All done"

%.py.png : %.py
	python $<


%.pdf : %.pandoc ./markov1.dot.pdf rook.dot.pdf king.dot.pdf bishop.dot.pdf
	pandoc2pdf.sh $<

%.dot.pdf : %.dot 
	dot -Tpdf -Goverlap=false $< > $@
