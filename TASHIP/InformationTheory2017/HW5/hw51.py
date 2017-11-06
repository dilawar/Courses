"""hw51.py: 

    Solution to problem 5.1.

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
import matplotlib.pyplot as plt
import numpy as np
import networkx as nx
from networkx.drawing.nx_agraph import graphviz_layout

def main( ):
    g = nx.DiGraph( )
    # Add x nodes.
    xs = np.arange(0, 11)
    ys = np.arange(0, 11)

    [ g.add_node( 'x=%d' % i, w = i ) for i in xs ]
    [ g.add_node( 'y=%d' % i, w = i ) for i in ys ]

    # Now add edges.
    img = { }
    for x in xs:
        for z in [1,2,3]:
            y = (x + z) % 11
            g.add_edge( 'x=%d' % x, 'y=%d' % y, label='z=%d' % z, prob=1/3)
            print( '%d + %d -> %d' % (x, z, y) )
            img[ (x, y) ] = 1/3

    mat = np.zeros( shape=(11,11) )
    for k, v in img.items( ):
        mat[ k ] = v

    nx.draw_networkx( g, pos = graphviz_layout( g, 'dot' ) )
    nx.drawing.nx_agraph.write_dot( g, 'network.dot' )
    plt.savefig( 'graph.png' )

    

if __name__ == '__main__':
    main()
