"""gnuplot.py: 

Gnuplot.

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
import time
import PyGnuplot as pg

# To plot the chart behind the scene.
gnuplot_script = '''
set term pngcairo size 20cm,20cm
set out "@filename@"
set grid
set border lw 1.5
set view 240,30
set title "@title@"
@LINES@
set ticslevel 0
splot "@datafile@" @plot_options@
set out
'''

init_ = False
tmp_file_ = 'tmp.dat' 

def init( ):
    # initlialize gnuplot.
    # Replot does not work in multiplot mode.
    with open( tmp_file_, 'w' ) as f:
        f.write( '1 1 1 1 1' )
    pg.c( 'set terminal x11 noraise' )
    pg.c( 'set xrange [0:500]; set yrange [0:500]; set zrange [0:500]' )
    pg.c( 'set ticslevel 0' )
    pg.c( 'set view 240,30' )
    pg.c( 'splot "%s" u 1:2:3:4:5 with p pt variable notitle' % tmp_file_ )
    return True

def plot( pos ):
    global init_
    if not init_:
        init( )
        init_ = True

    if len( pos ) < 1:
        print( 'No data' )
        return False

    with open( tmp_file_, 'w' ) as f:
        for l in pos:
            f.write( l )

    pg.c( 'replot' )
    return True
