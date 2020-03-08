__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2019-, Dilawar Singh"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"

import sys
import os
import matplotlib.pyplot as plt
import numpy as np

import sympy as S
import sympy.stats as SS

S.init_printing(use_unicode=True)
pprint = S.pprint


def test_bernoulli():
    # Binomial distribution
    b = SS.Bernoulli('b1', S.S(1)/2)
    print( SS.E(b) )
    print( list(SS.sample_iter(b, numsamples=10)) )

def Bernoulli():
    k, N, p = S.symbols('k N p')
    return S.binomial(N, k) * p ** k * (1-p)**(N-k)


def main():
    b = Bernoulli()
    N = 10
    b = b.subs('N', N)
    a = b.subs('k', 0)
    for k in range(1, N):
        a += b.subs('k', k)
    pprint(a)
    pprint('Simplified: %s' % S.simplify(a))



if __name__ == '__main__':
    main()

