"""
main.py: 

NOTE: Use PYPY to speed up the simulation. Python3/python2 gets slower with
time.
"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import time
import os
import numpy as np
import random
import shutil
import itertools
import PyGnuplot as pg

# To save the gnuplot.
import ajgar 

# 1 pixel is 1nm.
# camkii is 120A or 0.12 nm. So 1 pixel is good enough.
diam_ = 500
n_ = 500
nCaMKII_ = 200
grid_ = np.zeros( shape=(n_, n_, n_) )
active_ = [ ]
pos_ = [ ]
dt_ = 1e-3

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

image_dir_ = '_images'
#if os.path.exists( image_dir_ ):
#    shutil.rmtree( image_dir_ )
#os.makedirs( image_dir_ )
if not os.path.exists( image_dir_ ):
    os.makedirs( image_dir_ )

class Bistable( ):
    global n_, nCaMKII_

    def __init__( self, index ):
        self.id = index
        self.x = random.randint(1, n_-1)
        self.y = random.randint(1, n_-1)
        self.z = random.randint(1, n_-1)
        self.state = random.choice( [0, 1] )

    def __repr__(self):
        return ' %3d@%3d (%3d,%3d): UP=%f, DOWN=%f state=%d' % (
                self.id, self.z,self.x,self.y,self.pUp, self.pDown, self.state 
                )

    def step( self, allcam ):
        """
        CaMKII only moves if they are not active. 
        Just update the position, do not draw here.
        """
        global n_
        global pos_
        global dt_

        self.pDown = 0.01
        self.pUp = 0.01

        # Compute number of active neightbours.
        actneigh = 0      # I am my own neighbour for sure.
        pos_ = [ ]
        w = 24
        for c in allcam:
            # last entry is gnuplot point type: 6 and 7
            pos_.append( (c.x, c.y, c.z, c.state, c.state + 6 ) )
            if abs( c.z - self.z) <= w and abs( c.x - self.x) <= w \
                and abs( c.y - self.y) <= w and c.state == 1 \
                and c.id != self.id:
                    actneigh += 1

        # There are two version of this part of story:
        # 1. If I am active and my probability to go down reduced exponentially
        #    with the number of active neighbours. I use the factor of 10.
        # 2. subunit exchange have no effect on the probability of going down at
        # least in cytosol where PP1 is quite high. So on layer > 0, pDown
        # remains the same.
        # However on PSD, PP1 saturates binding to active CaMKII. Let's assume there are
        # fewer pp1 than number of CaMKII.
        if self.state == 1:
            thres = self.pDown
            ## On layer 0, if there are active neighbours, then reduce the pDown
            ## by a significant amount.
            if self.z > 0:
                # thres = self.pDown / (10.0**actneigh) # story 1
                thres = self.pDown                      # story 2
            else:
                thres = self.pDown / (1+2*actneigh)

            if random.random( ) < thres:
                self.state = 0

        elif self.state == 0:
            # Story 1: Probability to go up depends on number of active
            # neightbours.
            thres = self.pUp + 0.1 * actneigh
            if random.random() < thres:
                self.state = 1

        # When it is inactive it moves or it is active but not on layer 0 which
        # is PSD layer. In dt_ of 1ms, we might move ~100 nM.
        if self.state == 0 or (self.state == 1 and self.z > 0):
            ww = int( ( 1e-12 * dt_ ) ** 0.5 / 1e-9 )
            p = random.choice( [ 'x', 'y', 'z' ] )
            if p == 'x':
                self.x = max(0, min(n_-1, self.x + random.randint(-ww, ww)))
            elif p == 'y':
                self.y = max(0, min(n_-1, self.y + random.randint(-ww, ww)))
            else:
                self.z = max(0, min(n_-1, self.z + random.randint(-ww, ww)))
        else:
            pass

    def draw( self, grid  ):
        global n_, diam_
        plane = grid[:,:,self.z]
        if self.state == 0:
            c = max(50, int(100 * self.s))
            grid[self.x, self.y, self.z] = 50

        elif self.state == 1:
            grid[self.x, self.y,self.z] = 255

camkiis_ = [ Bistable(i) for i in range( nCaMKII_ ) ]
print( 'Total %d camkii constructed' % len(camkiis_) )

def init( ):
    global camkii_
    # initlialize gnuplot.
    with open( 'tmp.dat', 'w' ) as f:
        f.write( '1 1 1 1 1' )
    pg.c( 'set terminal x11' )
    pg.c( 'set xrange [0:500]; set yrange [0:500]; set zrange [0:500]' )
    pg.c( 'set ticslevel 0' )
    pg.c( 'set view 240,30' )
    pg.c( 'splot "tmp.dat" u 1:2:3:5 with p pt variable notitle' )

def save_gnuplot( i, outfile ):
    ajgar.gnuplot( gnuplot_script
            , filename = outfile
            , title = "Time=%.2f sec" % (i*dt_)
            , datafile = "tmp.dat"
            , plot_options = "using 1:2:3:5 with p pt variable"
            )
    print( 'Saved to %s' % outfile )

def simulate_step( i ):
    global camkiis_, grid_
    global dt_

    # Update the state of each camkii
    temp = camkiis_[:]
    [ cam.step( camkiis_ ) for cam in temp ]

    if i % 100 == 0:
        np.savetxt( "tmp.dat", np.array( pos_ ) )
        pg.c( 'set title "Time=%.3f"'% (i*dt_) )
        pg.c( 'replot' )

    if i % 100 == 0:
        outfile = os.path.join( image_dir_, '%08d.png' % i )
        save_gnuplot( i, outfile  )


def main( ):
    init( )
    ref = np.zeros( shape=(n_,n_) ) 
    for i in range( int( 30 * 60/dt_) ):
        simulate_step( i )

if __name__ == '__main__':
    try:
        main()
    except KeyboardInterrupt as e:
        print( 'Interrupt from keyboard' )
