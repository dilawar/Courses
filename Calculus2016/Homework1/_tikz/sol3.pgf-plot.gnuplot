set table "_tikz/sol3.pgf-plot.table"; set format "%.5f"
set format "%.7e";; set samples 1000; set dummy x; plot [x=-5:5]  abs(x**2-2*x-3) > x ? 1 : -1 ;
