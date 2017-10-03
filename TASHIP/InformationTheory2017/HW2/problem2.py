# Solution to problem2.

import numpy as np

def weighAndReduce( i, observation, coins, states ):
    leftI = coins[ np.arange(0, i) ]
    rightI = coins[ np.arange(i, 2*i) ]
    results = [ ]
    for s in states:
        left, right = s[leftI], s[rightI]
        leftW, rightW = sum( left ), sum( right )

        if observation == 'L': # left heavy
            if leftW > rightW:
                continue 
            results.append( s )

        elif observation == 'R': # right heavy
            if leftW < rightW:
                continue
            results.append( s )

        elif observation == 'B': # balanced
            if leftW == rightW:
                continue
            results.append( s )
    return results

def coinsToStr( coins ):
    res = ''
    for c in coins:
        if c == 1.0:
            res += '.'
        elif c > 1.0:
            res += 'h'
        elif c < 1.0:
            res += 'l'
    return res

def main( n = 12 ):
    coinIndices = np.arange( n )
    states = [ ]
    for i in range( n ):
        coins = [ 1.0 ] * n
        coins[i] = 1.1
        states.append( coins )

    for i in range( n ):
        coins = [ 1.0 ] * n
        coins[i] = 0.9
        states.append( coins )

    # all coins normal
    states += [ [ 1.0 ] * n ]

    for i in range( 1, 7 ):
        print( 'Weighing %d coins' % i )
        for obs in [ 'L', 'R', 'B' ]:
            res = weighAndReduce( i, obs, coinIndices, np.array(states) )
            res1 = list( map( lambda x: coinsToStr(x), res ) )
            print( obs, len(res) )


if __name__ == '__main__':
    main()
