all : ./hw61.py.png ./HW6.pdf
	@echo "All done"

%.py.png : %.py
	python $<


%.pdf : %.pandoc 
	pandoc2pdf.sh $<
