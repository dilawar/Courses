import numpy
import sys
from collections import defaultdict

class Brownian():

    """
    The equation is the following 

    dv = (-v/a) dt + alpha * sqrt(2*b*dt)

    alpha is distributed with mean 0 and variance 1.
    """

    def __init__(self, dt, initV = 0.0, runtime = 100.0):
        self.dt =  dt
        self.steps = int(runtime/dt)
        self.time = numpy.zeros(self.steps+1)
        self.pos = numpy.zeros(self.steps+1)
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
        for i, a in enumerate(self.alpha):
            t = t + self.dt
            v = self.computeV(v, a)
            self.time[i+1] = t
            self.pos[i+1] = self.pos[i] + v * self.dt
        self.finalV = v

def main():
    import pylab
    timesteps = [0.1, 0.01, 0.001]
    initV = [0, 10]
    # This is a dictionary with keys as (timestep,initV) and final velocity as
    # values.
    finalvecolicies = defaultdict(list)
    total = 1000
    for dt in timesteps:
        for v in initV:
            print("Solving for %s timestep and %s velocty" % (dt,v))
            print("++ Generating %s samples" % total)
            pylab.figure()
            for i in range(total):
                a = Brownian(dt, v)
                a.solve()
                finalvecolicies[(dt,v)].append(a.finalV)
                pylab.plot(a.time, a.pos)
            pylab.xlabel("Time: dt is %s" % dt)
            pylab.ylabel("Position when init velocity is %s" % v)
            print("++ Saving plot")
            pylab.savefig("plot_{}dt_{}initv.png".format(dt, v))

    with open("results_{}.txt".format(total), "w") as f:
        f.write("samples,dt,initv,mean,variance\n")
        for dt, v in finalvecolicies:
            vs = finalvecolicies[(dt, v)]
            f.write("%s,%s,%s,%s,%s"%(total,dt,v,numpy.mean(vs),numpy.std(vs)))
        print("All done")

if __name__ == '__main__':
    main()
