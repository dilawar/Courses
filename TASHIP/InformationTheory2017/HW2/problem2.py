# Solution to problem2.

import numpy as np
from collections import defaultdict
import math

def getAllStates( n ):
    states = [ ]
    for i in range( 0, n ):
        s = [ 'n' ] * n
        s[i] = 'h'
        states.append( s )
    for i in range( 0, n ):
        s = [ 'n' ] * n
        s[i] = 'l'
        states.append( s )
    states.append( [ 'n' ] * n )
    return list(map( lambda x: ''.join(x), states ))

states_ = getAllStates( 12 )

#print( 'Possible states' )
#for s in states_:
#    print( ''.join(s) )

def weigh( coins ):
    w = 0.0
    for c in coins:
        if c == 'h':
            w += 1.1
        elif c == 'l':
            w += 0.9
        else:
            w += 1.0
    return w

def entropy( freq ):
    probs = [ x / sum( freq ) for x in freq ]
    ent = [ - p * math.log( p, 2 ) for p in probs ]
    return sum( ent )

def pruneStates( k, outcome, df ):
    global states_
    for s in states_:
        leftC, rightC = s[:k], s[k:2*k]
        diff = weigh( rightC ) - weigh( leftC )
        if outcome == 'H':
            if diff > 0.0:
                df[ s ][ outcome ] = 'x'
            else:
                df[ s ][ outcome ] = '.'
        elif outcome == 'L':
            if diff < 0.0:
                df[ s ][ outcome ] = 'x'
            else:
                df[ s ][ outcome ] = '.'
        elif outcome == 'B':
            if diff == 0.0:
                df[ s ][ outcome ] = 'x'
            else:
                df[ s ][ outcome ] = '.'
    return df

def printTable( df ):
    for x in df:
        vals = list( df[x].values() )
        print( x, ' '.join(vals) )


def makeTable( k, n ):
    """
    K weighing. n coins. One counterfiet
    """
    global states_

    print( 'Weighing coins %d' % k )

    df = defaultdict( dict )
    for oc in [ 'H', 'L', 'B' ]:
        df = pruneStates( k, oc, df ) 

    printTable( df )

def main( n = 12 ):
    for k in range( 1, 3 ):
        makeTable( k, n )

if __name__ == '__main__':
    main()
