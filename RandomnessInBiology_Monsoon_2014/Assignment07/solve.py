
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
import sys

class GeneticSwitch():

    def __init__(self, k1k2=1e-4, step=1e-4, stop=1.0, init=0.0):
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
        self.x = init
        self.init = init

    def reinit(self):
        self.x = self.init
        self.time = 0.0
        self.currentStep = 0
        self.dx = 0.0
        self.alpha = numpy.random.normal(0, 1.0, self.totalSteps)

    def f(self, x):
        result = ((self.v0 + (self.v1 * self.k1k2 * (x**2.0))) / (1 + self.k1k2 * (x**2.0))) - self.gamma * x
        return result

    def g(self, x):
        result = ((self.v0 + (self.v1 * self.k1k2 * (x**2.0))) / (1 + self.k1k2 * (x**2.0))) + self.gamma * x
        return (result**0.5)


    def wienerTerm(self, x, dt = None):
        """A wiener term """
        if dt is not None:
            result = dt * self.alpha[self.currentStep] * self.g(x)
        else:
            result = self.step * self.alpha[self.currentStep] * self.g(x)
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

    def test(self, dx, wiener):
        ratio = dx / wiener
        if ratio > 100:
            print("[WARN] dx/wiener ratio is: %s" % ratio)

    def solveLangevian(self, withWeiner=True):
        # Solving Langevian equations.
        output = numpy.zeros(self.totalSteps)
        for i, e in enumerate(range(self.totalSteps)):
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
    gs = GeneticSwitch(k1k2=1e-6, step=1e-2, stop=100, init=0)

    # Let's calculate n trajectories of solution,
    n = 1
    collectedOutput = numpy.array([])
    cutoff = int(30.0 / gs.step)
    print("[INFO] cutting-off at {}".format(cutoff))
    for i in range(n):
        output = gs.solveLangevian()
        collectedOutput = numpy.append(collectedOutput, output[cutoff:])
        x = [ t * gs.step for t in range(len(output)) ]
        pylab.plot(x[cutoff:], output[cutoff:])
        print("|- mean: {}, std: {}".format(numpy.mean(output), numpy.std(output)))
        gs.reinit()

    # Here calculate steady state solution.
    output = gs.solveLangevian(withWeiner = False)
    pylab.plot(x[cutoff:], output[cutoff:])
    pylab.xlabel("time in sec")
    pylab.ylabel("x (protein number)")
    pylab.savefig('langevin_trajectories_{}.png'.format(n))

    pylab.figure()
    histOut = [ int(x) for x in collectedOutput]
    pylab.hist(histOut)
    pylab.show()


    #hist, bins = numpy.histogram(collectedOutput) #, bins=20)
    #pylab.figure()
    #pylab.plot(hist)
    #pylab.savefig('distibution_{}.png'.format(n))

if __name__ == '__main__':
    main()
