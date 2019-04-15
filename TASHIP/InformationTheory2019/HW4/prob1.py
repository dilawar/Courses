"""prob1.py: 

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import networkx as nx
import math
import numpy as np

def entropy(pis):
    e = 0.0
    for p in pis:
        if p > 0:
            e -= p * math.log(p, 2)
    return e

def main():
    g = nx.DiGraph(nx.drawing.nx_agraph.read_dot('./ss.dot'))
    for s, t in g.edges():
        g[s][t]['prob'] = eval(g[s][t]['label'])
    eAttrMat, cols = nx.attr_matrix(g, 'prob')
    print(cols)
    u = np.ravel((eAttrMat ** 100)[0,:])
    print(eAttrMat)
    print( '\nStationary distribution:' )
    print(cols)
    print(u)
    rate = 0.0
    for i, ui in enumerate(u):
        pis = np.ravel(eAttrMat[i,:])
        rate += ui * entropy(pis)
    print( f'\nRate is: {rate:.3f} bits per symbol' )

if __name__ == '__main__':
    main()

