
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
        self.increasing = True

    def reinit(self):
        self.x = self.initx
        self.time = 0.0
        self.currentStep = 0
        self.dx = 0.0
        self.alpha = numpy.random.normal(0, 1.0, self.totalStpng)
        self.stateBTimes = []

    def f(self, x):
        result = ((self.v0 + (self.v1 * self.k1k2 * (x**2.0))) / (1 + self.k1k2 * (x**2.0))) - self.gamma * x
        return result

    def g(self, x, k = 1):
        result = ((self.v0 + (self.v1 * self.k1k2 * (x**2.0))) / (1 + self.k1k2 * (x**2.0)))  + self.gamma * x
        result = self.x 
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
        return dx

    def solveLangevian(self):
        # Solving Langevian equations.
        output = numpy.zeros(self.totalStpng)
        for i, e in enumerate(range(self.totalStpng)):
            dx = self.dxTerm(self.x)
            wiener = self.wienerTerm(self.x)
            if dx + wiener < 0.0:
                self.increasing  = False
            else:
                self.increasing = True
            self.x += (dx + wiener)
            output[i] = self.x
            if self.x >= self.stateBThreshold:
                if self.increasing:
                    self.stateBTimes.append(self.time)
            self.time += self.step
            self.currentStep += 1
        return output

def main():
    import pylab
    timeToGotoBFirstTime = []
    gs = GeneticSwitch(k1k2=1e-4, step=0.01, stop=1000, init=0)
    
    # Trajectory of protein when its # goes from 0 to steady state value is not
    # so exciting, let's chop it off.
    cutoff = int(30.0 / gs.step)
    print("[INFO] cutting-off at {}".format(cutoff))

    # Let's calculate n trajectories of solution,
    n = 1
    collectedOutput = numpy.array([])
    for i in range(n):
        output = gs.solveLangevian()
        collectedOutput = numpy.append(collectedOutput, output[cutoff:])
        x = [ t * gs.step for t in range(len(output)) ]
        pylab.plot(x[cutoff:], output[cutoff:])
        print("|- mean: {}, std: {}".format(numpy.mean(output), numpy.std(output)))
        if len(gs.stateBTimes) > 0:
            timeToGotoBFirstTime.append(gs.stateBTimes[0])
        gs.reinit()

    pylab.xlabel("time in sec")
    pylab.ylabel("x (protein number)")
    pylab.title("#protein: Using Langevian for k1k2={}".format(gs.k1k2))
    pylab.savefig('langevin_trajectories_{:.2e}.png'.format(gs.k1k2))

    pylab.figure()
    hist, bins = numpy.histogram(collectedOutput, bins=100)
    pylab.bar(bins[:-1], hist)
    pylab.xlabel("#protein (x)")
    pylab.ylabel("No of times x protein is seen")
    pylab.title(
            "Distribution of #protein for k1k2={}, simulation time={} sec".format(
                gs.k1k2, gs.stop
                )
            )
    pylab.savefig('distibution_{:.2e}.png'.format(gs.k1k2))
    #print numpy.mean(timeToGotoBFirstTime)

if __name__ == '__main__':
    main()
