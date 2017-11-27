all : ./hw7.pdf problem2.pdf

%.pdf : %.pandoc problem2.pdf 
	pandoc2pdf.sh $<

problem2.pdf : ./solveb.py
	python $<
