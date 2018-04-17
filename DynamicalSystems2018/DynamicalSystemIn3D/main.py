#!/usr/bin/env python3
"""
3d Arena Model.

Arena dimensions: 500nM x 500 nM x 500 nM. Periodic boundary. 

Efficiency tip: Use PYPY to speed up the simulation. Python3/python2 gets slower 
with time.
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
from species import Jugnoo
import gnuplot

def distance( s1, s2 ):
    x1, y1, z1 = s1.pos
    x2, y2, z2 = s2.pos
    d = ((x1-x2)**2 + (y1-y2)**2 + (z1-z2)**2) ** 0.5
    return d

class Arena( ):
    """docstring for ClassName"""

    def __init__(self, x = 500, y = 500, z = 500):
        self.x, self.y, self.z = x, y, z
        self.grid = np.zeros( shape=(self.x, self.y, self.z) )
        self.t, self.dt  = 0, 1e-4
        self.gnuplotL = [ ]
        self.species = { }
        self.fracUp = 0.0

    def add_species(self, name, N, px = 10 ):
        speciess = [ ]
        for i in range( N ):
            s = Jugnoo( 'jugnoo:%d' % i, px )
            s.random_location( self.x, self.y, self.z )
            speciess.append( s )
        self.species[ 'jugnoo' ] = speciess

    def species_interaction( self ):
        allspecies = self.species[ 'jugnoo' ]
        nUp = 0
        for c1 in allspecies:
            if c1.state:
                nUp += 1.0
        self.fracUp = nUp / len(allspecies)
        #  print( nUp )

        for c1 in allspecies:
            c1.pUp = c1._pUp * (1+nUp/9)

        #  print( [ x.pUp for x in allspecies ] )

    def update_all_species( self ):
        self.gnuplotL = [ ]
        for k in self.species:
            for m in self.species[ k ]:
                m.update( self.dt )
                line = ' '.join( [ '%g' % x for x in m.pos ] )
                line += ' %d %d\n' % (m.ptype, m.size)
                self.gnuplotL.append( line )
        
        self.species_interaction( )
        return self.gnuplotL

    def step( self, time ):
        self.t += self.dt 
        self.update_all_species( )

    def plot( self ):
        gnuplot.plot( self.gnuplotL )

def main( ):
    arena = Arena( )
    arena.add_species( 'jugnoo', 100 )
    for i in range( 10**5 ):
        arena.step( time = 1e-4 * i )
        if i % 10 == 0:
            print( 'Step %d' % i )
            arena.plot( )

if __name__ == '__main__':
    try:
        main()
    except KeyboardInterrupt as e:
        print( 'Interrupt from keyboard' )
