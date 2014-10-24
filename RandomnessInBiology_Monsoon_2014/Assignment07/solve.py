
"""solve.py: Soluting to homework 7.

Last modified: Sat Jan 18, 2014  05:01PM

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2013, Dilawar Singh and NCBS Bangalore"
__credits__          = ["NCBS Bangalore"]
__license__          = "GNU GPL"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import numpy

class GeneticSwitch():

    def __init__(self, k1k2=1e-4, step=1e-4, stop=1.0):
        print("Creating genetic switch")
        self.v0 = 12.5
        self.v1 = 200
        self.gamma = 1
        self.k1k2 = k1k2
        self.dx = 0.0
        self.stop = stop
        self.totalSteps = int(self.stop/step)
        self.alpha = numpy.random.normal(0, 1.0, self.totalSteps)
        self.step = step
        self.time = 0.0
        self.x = 0.0

    def weinerTerm(self,  k = 1.0):
        """A weiner term """
        return self.step * self.alpha[self.step] * k * (self.time ** 0.5)

    def dxTerm(self, x, dt=1e-6):
        # Setup the derivative 
        dx = ((self.v0 + (self.v1 * self.k1k2 * (x**2.0))) / (1 + self.k1k2 * (x**2.0))) - self.gamma * x
        self.dx = dt * dx
        return self.dx

    def solveLangevian(self):
        # Solving Langevian equations.
        output = numpy.zeros(self.totalSteps)
        for i, e in enumerate(range(self.totalSteps)):
            dx = self.dxTerm(self.x)
            weiner = self.weinerTerm(k = self.x)
            self.x += (dx + weiner)
            output[i] = self.x
            self.time += self.step
            self.step += 1
        return output

def main():
    import pylab
    gs = GeneticSwitch(step=1e-5, stop=1e-1)
    output = gs.solveLangevian()
    pylab.plot(output)
    pylab.show()


if __name__ == '__main__':
    main()
