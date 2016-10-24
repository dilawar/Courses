set table "homework1.pgf-plot.table"; set format "%.5f"
set format "%.7e";; set samples 1000; set dummy x; plot [x=-5:5]  x > (4.166 + 3)/2 || x < (-4.166+3)/2? 0 : -10000 ;
