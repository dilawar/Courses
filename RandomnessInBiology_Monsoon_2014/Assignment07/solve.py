
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
        self.step = step
        self.totalSteps = int(self.stop/self.step)
        self.alpha = numpy.random.normal(0, 1.0, self.totalSteps)
        self.time = 0.0
        self.currentStep = 0
        self.x = 0.0

    def reinit(self):
        self.x = 0.0
        self.time = 0.0
        self.currentStep = 0
        self.dx = 0.0

    def weinerTerm(self,  k = 1.0):
        """A weiner term """
        return self.alpha[self.currentStep] * k * (self.step ** 0.5)

    def dxTerm(self, x, dt=None):
        # Setup the derivative 
        dx = ((self.v0 + (self.v1 * self.k1k2 * (x**2.0))) / (1 + self.k1k2 * (x**2.0))) - self.gamma * x
        if dt is not None:
            dx = dt * dx
        else:
            dx = self.step * dx
        self.dx = dx
        return dx

    def test(self, dx, weiner):
        ratio = dx / weiner
        if ratio > 100:
            print("[WARN] dx/weiner ratio is: %s" % ratio)

    def solveLangevian(self, withWeiner=True):
        # Solving Langevian equations.
        output = numpy.zeros(self.totalSteps)
        for i, e in enumerate(range(self.totalSteps)):
            dx = self.dxTerm(self.x)
            if withWeiner:
                weiner = self.weinerTerm(k = 1)
                self.x += (dx + weiner)
            else:
                self.x += dx
            output[i] = self.x
            self.time += self.step
            self.currentStep += 1
        return output

def main():
    import pylab
    gs = GeneticSwitch(step=1e-3, stop=50)
    output = gs.solveLangevian()
    x = [ t * gs.step for t in range(len(output)) ]
    pylab.plot(x, output)
    gs.reinit()
    output = gs.solveLangevian(withWeiner = False)
    pylab.plot(x, output)
    pylab.xlabel("time in sec")
    pylab.ylabel("x (protein number)")
    pylab.show()


if __name__ == '__main__':
    main()
