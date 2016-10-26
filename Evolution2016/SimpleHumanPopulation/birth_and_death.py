"""birth_and_death.py: 

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2016, Dilawar Singh"
__credits__          = ["NCBS Bangalore"]
__license__          = "GNU GPL"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
import matplotlib as mpl
import matplotlib.pyplot as plt
try:
    mpl.style.use( 'seaborn-talk' )
except Exception as e:
    pass
mpl.rcParams['axes.linewidth'] = 0.1
plt.rc('text', usetex=True)
plt.rc('font', family='serif')

import matplotlib.pyplot as plt
import numpy as np
import random

id_ = 0

class Specimen( object ):

    def __init__(self, age = -1 ):
        global id_
        id_ += 1
        self.id = id_
        if age < 0:
            self.age = random.uniform(0, 100 )
        else:
            self.age = age
        self.mate = self.canMate( )

    def __repr__( self ):
        return ' Age : %.2f' % self.age


    def canMate( self ):
        if self.age > 20 and self.age < 40:
            return True
        else:
            return False

class Male( Specimen ):
    """docstring for Male"""

    def __init__(self, age = -1):
        super(Male, self).__init__( age )

    def __repr__( self):
        return 'Male ' + super(Male, self).__repr__( )

class Female( Specimen ):
    """docstring for Female"""
    def __init__(self, age = -1):
        super(Female, self).__init__( age )
        self.nChildren = 0
        self.maxChildren = np.random.normal( 2, 1, 1 )[0]

    def __repr__(self):
        return 'Female ' + super(Female, self).__repr__( )

    def canBearChildren( self ):

        if self.age < 20 or self.age > 40:
            return False

        if self.nChildren >= self.maxChildren:
            return False

        return True





class Population( Male, Female):
    """ A population with N males and M females.
    """

    def __init__(self, nmales, nfemales):
        self.males = [ Male( ) for i in range( nmales ) ]
        self.females = [ Female( ) for i in range( nfemales ) ]
        self.all = self.males + self.females
        self.N = len( self.males ) + len( self.females )


    def getAgesOfAll( self ):
        return [ x.age for x in self.all ]

    def birth( self, pmale = 0.5 ):
        ''' Mate and reproduce 

        Birth is female limited. We assume that there is always a male available
        for mating.
        
        '''
        ms, fs = self.malesFemales( )
        # print( '[DBEUG] %d females and %d males can mate' % (len(fs), len(ms)))
        total = 0
        before = len( self.all )
        for f in fs:
            if not f.canBearChildren( ):
                continue
            f.nChildren += 1
            total += 1
            if random.random() <= pmale: 
                self.all.append( Male(0) )
            else: 
                self.all.append( Female(0) )
        return total 


    def death( self ):
        ripened = np.random.normal( 80, 20, len(self.all) )
        ages = np.array( self.getAgesOfAll( ) )
        toKill = np.where( ages > ripened )[0]
        survived = [ self.all[i] for i, j in enumerate( self.all ) if i not in toKill ] 
        self.all = survived
        self.males, self.females = self.malesFemales( )
        return len( toKill )

    def updateAge( self ):
        ''' Increase the age by one.
        '''
        for x in self.all:
            x.age += 1

    def malesFemales( self ):
        '''Filter out Males from population 
        '''
        males, females = [], []
        for x in self.all:
            if isinstance( x, Male ):
                males.append( x )
            else:
                females.append( x )
        return males, females

    def getFemales( self ):
        '''Filter out Males from population 
        '''
        return filter( lambda x: isinstance(x, Female), self.all )


    def __str__( self ):
        msg = '<Population> N = %3d,' % len(self.all)
        males, females = self.malesFemales( )
        msg += ' %d males, %d females' % (len(males), len(females))
        return msg
        

def main( ):
    p = Population( 10000, 10000 )
    pop = []
    for i in range( 1000 ):
        b = p.birth( pmale = 0.5 )
        d = p.death( )
        p.updateAge( )
        print( 'step %d : births %d, deaths %d' % (i, b, d ) )
        pop.append( len(p.all) )
    plt.plot( pop )
    plt.xlabel( 'Steps (years)' )
    plt.ylabel( 'Population' )
    plt.savefig( 'population.png' )


if __name__ == '__main__':
    main()
