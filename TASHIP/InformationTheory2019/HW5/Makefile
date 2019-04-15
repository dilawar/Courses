all : HW5.pdf

HW5.pdf : *.tex network.png
	lualatex --shell-escape $<

network.png : network.dot
	dot -Tpng $< > $@

