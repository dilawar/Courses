set table "hw7.pgf-plot.table"; set format "%.5f"
set format "%.7e";; H(x) = - x * log(x) / log(2) - 2*x * log(2*x)/log(2) - 4* (1-3*x)/4 * log((1-3*x)/4)/log(2); plot [x=0.001:0.3] H(x); 
