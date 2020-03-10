__author__ = "Dilawar Singh"
__copyright__ = "Copyright 2019-, Dilawar Singh"
__maintainer__ = "Dilawar Singh"
__email__ = "dilawars@ncbs.res.in"

import sympy as S
import sympy.stats as SS

S.init_printing(use_unicode=True)
pprint = S.pprint

k, N, p, n = S.symbols('k N p n', integers=True)


def test_bernoulli():
    # Binomial distribution
    b = SS.Bernoulli('b1', S.S(1) / 2)
    print(SS.E(b))
    print(list(SS.sample_iter(b, numsamples=10)))


def binomial():
    return S.binomial(N, k) * p**k * (1 - p)**(N - k)


def xProb1(a):
    return a * binomial()


def xxProb1(a):
    return a * xProb1(a)


def main():
    pb1 = S.Sum(xProb1(k), (k, 0, N)).doit()
    pb2 = S.Sum(xxProb1(k), (k, 0, N)).doit()
    pprint(S.simplify(pb1))
    pprint(S.simplify(pb2))


if __name__ == '__main__':
    main()
