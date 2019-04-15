"""prob1.py: 

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import networkx as nx

def main():
    g = nx.DiGraph(nx.drawing.nx_agraph.read_dot('./ss.dot'))
    for s, t in g.edges():
        g[s][t]['prob'] = eval(g[s][t]['label'])
    eAttrMat, label = nx.attr_matrix(g, 'prob')
    print(eAttrMat)

if __name__ == '__main__':
    main()

