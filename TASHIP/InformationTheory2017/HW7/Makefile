all : ./hw7.pdf ./problem2.pdf ./_typeclass250.txt

%.pdf : %.pandoc problem2.pdf ./_typeclass250.txt
	pandoc2pdf.sh $<

problem2.pdf : ./solveb.py
	python $<

./_typeclass250.txt : ./solvec.py
	python $<
