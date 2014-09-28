import numpy
import sys

class Brownian():

    """
    The equation is the following 

    dv = (-v/a) dt + alpha * sqrt(2*b*dt)

    alpha is distributed with mean 0 and variance 1.
    """

    def __init__(self, dt, initV = 0.0, runtime = 100.0):
        self.dt =  dt
        self.steps = int(runtime/dt)
        self.time = numpy.array(self.steps)
        self.a = 1.0 
        self.b = 1.0
        self.alpha = numpy.random.normal(0.0, 1.0, self.steps)
        self.initV = initV
        self.finalV = 0.0

    def computeV(self, v, alpha):
        dv = (- v / self.a * self.dt) + (alpha * numpy.sqrt(2.0 * self.b * self.dt))
        return (v + dv)

    def solve(self):
        t = 0.0
        v = self.initV
        for a in self.alpha:
            t = t + self.dt
            v = self.computeV(v, a)
        self.finalV = v

def main():
    import pylab
    velocities = []
    for i in range(100):
        a = Brownian(1e-3)
        a.solve()
        velocities.append(a.finalV)
    print velocities
    pylab.hist(velocities)
    pylab.show()


if __name__ == '__main__':
    main()
