#!/usr/bin/env python
from sympy.solvers import solve
from sympy import Symbol

_pi = Symbol('pi')
r = Symbol('r')                        # Radius of axon
rm = Symbol('rm')                      # Specific membrane resistance, Ohm.m^2
ra = Symbol('ra')                      # Specific axial resistivity, Ohm.m

length = 1e0                           # m
area = _pi * r * r                 # m^2
surfaceArea = 2 * _pi * r * length

RM = 1.0
f2 = (RM * length / surfaceArea) - rm  # It computes surface area of axon.

RA = 1.0
f3 = ((RA / area) * length) - ra       # Computes axial resistance of axon.

va = 60e-3
vb = 22e-3
f1 = (rm / (rm + ra)) - (vb / va)

sol = solve((f1, f2, f3), (ra, rm, r))
print(sol[0][2])
