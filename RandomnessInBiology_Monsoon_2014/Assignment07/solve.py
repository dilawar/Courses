
"""solve.py: Soluting to homework 7.

Last modified: Tue Oct 28, 2014  11:29PM

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
import sys

class GeneticSwitch():

    def __init__(self, k1k2=1e-4, step=1e-4, stop=1.0, init=0.0):
        print("+ Creating genetic switch")
        self.v0 = 12.5
        self.v1 = 200
        self.gamma = 1
        self.k1k2 = k1k2
        self.dx = 0.0
        self.stop = stop
        self.step = step
        self.totalStpng = int(self.stop/self.step)
        self.alpha = numpy.random.normal(0, 1.0, self.totalStpng)
        self.time = 0.0
        self.currentStep = 0
        self.x = init
        self.stateBThreshold = 50.0
        self.stateBTimes = []
        self.initx = init

    def reinit(self):
        self.x = self.initx
        self.time = 0.0
        self.currentStep = 0
        self.dx = 0.0
        self.alpha = numpy.random.normal(0, 1.0, self.totalStpng)

    def f(self, x):
        result = ((self.v0 + (self.v1 * self.k1k2 * (x**2.0))) / (1 + self.k1k2 * (x**2.0))) - self.gamma * x
        return result

    def g(self, x, k = 1.0):
        result = ((self.v0 + (self.v1 * self.k1k2 * (x**2.0))) / (1 + self.k1k2 * (x**2.0)))  + self.gamma * x
        return k*result**0.5


    def wienerTerm(self, x, dt = None):
        """A wiener term """
        if dt is not None:
            result = (self.dt**0.5) * self.alpha[self.currentStep] * self.g(x)
        else:
            result = (self.step**0.5) * self.alpha[self.currentStep] * self.g(x)
        return result

    def dxTerm(self, x, dt=None):
        # Setup the derivative 
        dx = self.f(x)
        if dt is not None:
            dx = dt * dx
        else:
            dx = self.step * dx
        self.dx = dx
        if self.x >= self.stateBThreshold:
            self.stateBTimes.append(self.time)
        return dx

    def solveLangevian(self, withWeiner=True):
        # Solving Langevian equations.
        output = numpy.zeros(self.totalStpng)
        for i, e in enumerate(range(self.totalStpng)):
            dx = self.dxTerm(self.x)
            if withWeiner:
                wiener = self.wienerTerm(self.x)
                self.x += (dx + wiener)
            else:
                self.x += dx
            output[i] = self.x
            self.time += self.step
            self.currentStep += 1
        return output

def main():
    import pylab
    gs = GeneticSwitch(k1k2=1e-4, step=1e-1, stop=10000, init=0)

    # Let's calculate n trajectories of solution,
    n = 1
    collectedOutput = numpy.array([])
    cutoff = int(30.0 / gs.step)
    cutoff = 0
    print("[INFO] cutting-off at {}".format(cutoff))
    for i in range(n):
        output = gs.solveLangevian()
        collectedOutput = numpy.append(collectedOutput, output[cutoff:])
        x = [ t * gs.step for t in range(len(output)) ]
        pylab.plot(x[cutoff:], output[cutoff:])
        print("|- mean: {}, std: {}".format(numpy.mean(output), numpy.std(output)))
        gs.reinit()

    ## Here calculate steady state solution.
    #output = gs.solveLangevian(withWeiner = False)
    #pylab.plot(x[cutoff:], output[cutoff:])
    pylab.xlabel("time in sec")
    pylab.ylabel("x (protein number)")
    pylab.savefig('langevin_trajectories_{}.png'.format(gs.k1k2))

    #pylab.figure()
    #histOut = [ int(x) for x in collectedOutput]
    #pylab.hist(histOut)
    #pylab.show()

    pylab.figure()
    hist, bins = numpy.histogram(collectedOutput, bins=100)
    pylab.bar(bins[:-1], hist, width=1)
    #pylab.hist(collectedOutput)
    pylab.savefig('distibution_{}.png'.format(gs.k1k2))
    #print gs.stateBTimes

if __name__ == '__main__':
    main()
