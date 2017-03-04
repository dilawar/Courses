import random
import sys
import numpy as np
import pylab
from collections import Counter

totalThrow = 10**5
dice1 = np.random.random_integers( 1, 6, totalThrow )
dice2 = np.random.random_integers( 1, 6, totalThrow )
bothDice = dice1 + dice2

# Or you can use np.hist function.
count = Counter( bothDice )

values = count.keys( )
freqs = [ float(x) / totalThrow for x in count.values( ) ]
pylab.bar( values, freqs )
# pylab.hist( both, bins = both.max( ) - both.min( ) + 1, normed = True )
pylab.savefig( '%s.png' % sys.argv[0] )
pylab.show( )
