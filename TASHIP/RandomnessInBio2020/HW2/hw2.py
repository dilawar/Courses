__author__ = "Dilawar Singh"
__copyright__ = "Copyright 2019-, Dilawar Singh"
__maintainer__ = "Dilawar Singh"
__email__ = "dilawars@ncbs.res.in"

import sys
import os
import matplotlib.pyplot as plt
from sympy import *

init_printing()

N, k = symbols("N k")
expr = binomial(N, (N - k) / 2) * (1 / 2 ** N)
pprint(expr)

# We right the expression manually since we are going to do Stirling
# approximation.
expr = factorial(N) / factorial((N-k)//2) / factorial((N+k)//2) * 2**(-N)
pprint("\n\n= Expression ===")
pprint(expr)
expr = expr.replace(factorial, lambda N: exp(N*log(N)-N))
pprint("\n\n= Expression (subs)===")
pprint(expr)
pprint("\nExpand==")
pprint(expr.simplify())

