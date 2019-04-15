all : hw4.pdf

hw4.pdf : hw4.tex ss.pdf
	lualatex -shell-esacape $<

ss.pdf : ss.dot
	dot -Tpdf $< > $@

