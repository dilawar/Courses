"""species.py: 

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
import random

class Species( ):
    # Species.
    def __init__(self, name,  px, pos = [0,0,0] ):
        self.name = name
        self.size = px
        self.boundary = [100,100,100]
        self.pos = [0.0, 0.0, 0.0]
        self.D = 5e-15
        self.ptype = 0

    def random_location( self, xmax, ymax, zmax ):
        self.boundary = [ xmax, ymax, zmax ]
        self.pos = [ random.randint(0, a-1) for a in [xmax,ymax,zmax] ]

    def set_diffusion( self, d ):
        # In m^2/sec
        self.D = d

    def diffuse_step( self, dt = 1e-3 ):
        # Step with size of dt.
        assert dt > 0.0
        if self.D <= 1e-19:
            return 
        ndir = random.randint(0, 2)
        dpx =  int( round((( self.D * dt ) ** 0.5 ) / 1e-9 ))
        if dpx < 1:
            return 

        assert dpx > 0, "No movement. Diffusion is way to small."
        self.pos[ndir] = self.pos[ndir] + random.choice([dpx,-dpx])
        if self.pos[ndir] < 0:
            self.pos[ndir] = self.boundary[ndir] - self.pos[ndir]
        elif self.pos[ndir] >= self.boundary[ndir]:
            self.pos[ndir] -= self.boundary[ndir]

class Jugnoo( Species ):

    def __init__( self, name, px, pos = [0,0,0] ):
        Species.__init__( self, name, px, pos )
        self.state = random.choice([True,False])
        self.ptype = 7 if self.state else 0
        self._pUp, self._pDown = 1e-3, 1e-2
        self.pUp, self.pDown = self._pUp, self._pDown 

    def turn_on( self ):
        self.state = True
        self.ptype = 7
    
    def turn_off( self ):
        self.state = False
        self.ptype = 0

    def is_active( self ):
        return self.state

    def update_state( self ):
        x = random.random( )
        if not self.is_active( ):
            if x < self.pUp: 
                self.turn_on( )
        else:
            if x < self.pDown: 
                self.turn_off( )

    def update( self, dt ):
        self.update_state( )
        self.diffuse_step( dt )
