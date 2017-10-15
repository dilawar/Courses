import math
import numpy as np

fact = math.factorial

N = 12
ps = [ 1/2.0, 1/3.0, 1/6.0 ]
nTs = fact(N ) / fact(N/2) / fact(N/3) / fact(N/6)
print( 'Total TS: %d' % nTs )
pTs = np.prod( [ x ** (N*x) for x in ps ] )
print('Prob TS: %s' % pTs )
print( 'Prob of all TS: %f' % (pTs * nTs) )
