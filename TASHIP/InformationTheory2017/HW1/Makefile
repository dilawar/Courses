doc : ./hw1.pdf  solve 


solve : ./problem1.hs 
	ghc $< -o prb1 && ./prb1 


%.pdf : %.pandoc 
	~/Scripts/pandoc2pdf.sh $<
