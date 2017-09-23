import random
coins = 'n' * 11 + 'x'

#N = 10 ** 6
#for c in range( 1, 12 ):
#    count = 0.0
#    for i in range( N ):
#        x = random.sample( coins, c )
#        if 'x' in x:
#            count += 1.0
#    print( c, count / N )

def p(k):
    return 1.0 * k / 12.0

def w(k):
    return p(k) * 2*k + (1.0 - p(k) ) * (12.0 - 2*k)

print [ p(x) for x in range(1,12) ]
print [ w(x) for x in range(0,13) ]
