import os
import sys
import math
import sympy as s

class Compartment(object):
    def __init__(self, RA=1.0, RM=1.0, dx=1e-2,r=1e-3):
        self.RA = RA
        self.dx = dx
        self.RM = RM
        self.length = 1.0
        self.radius = r
        self.calcCrossArea()
        self.calcSurfaceArea()

    def calcCrossArea(self):
        '''Calc the cross-section area of cylinder '''
        self.crossArea = math.pi * self.radius * self.radius

    def calcSurfaceArea(self):
        '''Calculate the surface area of cylinder '''
        self.surfaceArea = 2 * math.pi * self.radius * self.length

    def setLenght(self, l):
        self.length = l

    def setRadius(self, r):
        self.radius = r

    def calcRa(self):
        ra = (self.RA * self.length) / self.crossArea
        assert ra > 0.0 
        return ra

    def calcRm(self):
        rm = self.RM / self.surfaceArea 
        assert rm > 0.0
        return rm

class Axon(Compartment):
    '''Class of an Axon
    '''
    def __init__(self):
        self.compartments = []
        self.equations = []
        # Boundary conditions
        self.endA = 0.0
        self.endB = 0.0
        self.vars = dict()
        super(Axon, self).__init__()

    def makeSympyEq(self, c, id):
        rm = c.calcRm()
        ra = c.calcRa()
        v1Name = 'c{}v'.format(id-1)
        v2Name = 'c{}v'.format(id)
        if self.vars.get(v1Name, None) is None:
            v1 = s.Symbol(v1Name)
            self.vars[v1Name] = v1
        else: 
            v1 = self.vars[v1Name]

        if self.vars.get(v2Name, None) is None:
            v2 = s.Symbol(v2Name)
            self.vars[v2Name] = v2
        else:
            v2 = self.vars[v2Name]
        f = ((rm / (rm + ra)) * v1) - v2
        self.equations.append(f)


    def setInitCondition(self, v1, v2, totalCompts):
        '''Given total number of compartments in axon, force conditions on first
        and last compartments.
        '''
        print("Setting up initial conditions")
        print self.vars
        self.equations.append(self.vars['c0v'] - v1)
        self.equations.append(self.vars['c{0}v'.format(totalCompts)] - v2)

    def solve(self, totalCompts):
        self.setInitCondition(60e-3, 38e-3, totalCompts)
        print self.equations
        sol = s.solvers.solve(self.equations, check=False)
        print sol


if __name__ == "__main__":
    axon = Axon()
    dx = 0.50
    totalCompt = int(1/dx)
    r = s.Symbol('r')
    #axon.vars['r'] = r
    for i in range(totalCompt):
        c = Compartment(dx=dx,r=r)
        axon.makeSympyEq(c, i+1)
    axon.solve(totalCompt)
    
