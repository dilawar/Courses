set table "hw7.pgf-plot.table"; set format "%.5f"
set format "%.7e";; plot "./results.txt" using (column("n")):(column("n1")) with p; 
