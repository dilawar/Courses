
"""solve.py: Soluting to homework 7.

Last modified: Thu Oct 30, 2014  07:27PM

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2013, Dilawar Singh and NCBS Bangalore"
__credits__          = ["NCBS Bangalore"]
__license__          = "GNU GPL"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import numpy as np
import sys
import matplotlib as mpl
import matplotlib.pyplot as plt

class GeneticSwitch():

    def __init__(self, k1k2=1e-4, init=0.0):
        self.v0, self.v1, self.gamma, self.k1k2 = 12.5, 200, 1, k1k2
        self.dx = 0.0
        self.time = 0.0
        self.currentStep = 0
        self.x = init
        self.initx = init
        # These are computed trajectories.
        self.trajectories = []
        # Time when we read state B first time
        self.timeToCrossStateBFirstTime = []
        self.stateAThreshold = 20.0
        self.stateBThreshold = 120.0
        self.stateBTimes = []
        self.whichState = 0

    def reinit(self):
        self.x = self.initx
        self.time = 0.0
        self.currentStep = 0
        self.dx = 0.0
        self.alpha = np.random.normal(0, 1.0, self.totalSteps)
        self.stateBTimes = []

    def f(self, x):
        result = ((self.v0 + (self.v1 * self.k1k2 * (x**2.0))) / (1 + self.k1k2 * (x**2.0))) - self.gamma * x
        return result

    def g(self, x, k = 1):
        #result = ((self.v0 + (self.v1 * self.k1k2 * (x**2.0))) / (1 + self.k1k2 * (x**2.0)))  + self.gamma * x
        result = self.x 
        return k*result**0.5

    def wienerTerm(self, x, dt = None):
        """A wiener term """
        # In some simulations, we might run out of random number. IN such case,
        # start fetching the number from begining.

        alphaIndex = self.currentStep % len(self.alpha)
        if dt is not None:
            result = (self.dt**0.5) * self.alpha[alphaIndex] * self.g(x)
        else:
            result = (self.step**0.5) * self.alpha[alphaIndex] * self.g(x)
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

    def solveLangevian(self, totalSteps = None):
        # Solving Langevian equations.
        if not totalSteps:
            totalSteps = self.totalSteps
        output = np.zeros(totalSteps)
        for i, e in enumerate(range(totalSteps)):
            dx = self.dxTerm(self.x)
            wiener = self.wienerTerm(self.x)
            self.x += (dx + wiener)
            output[i] = self.x
            self.time += self.step
            self.currentStep += 1
        return output

    def solveLangevianForTransitions(self, transitions):
        """ Keep solving till we get enought transitions """
        f = open("transition_time.txt", "a")
        f.write("\n\n")
        self.transitionTime = []
        while len(self.transitionTime) < transitions:
            dx = self.dxTerm(self.x)
            wiener = self.wienerTerm(self.x)
            self.x += (dx + wiener)
            self.time += self.step
            if self.x < self.stateAThreshold:
                if self.whichState == 1:
                    print("|| %s Transition High -> Log" % self.time)
                    f.write("%s\n" % self.time)
                    self.transitionTime.append(self.time)
                self.whichState  = 0
            elif self.x > self.stateBThreshold:
                if self.whichState == 0:
                    print("|| %s Transition Low -> High" % self.time)
                    f.write("%s\n" % self.time)
                    self.transitionTime.append(self.time)
                self.whichState = 1
            self.currentStep += 1
        f.close()

    def run(self, step = 0.1, stop = 1000, ntimes = 1):
        self.step = step
        self.stop = stop
        self.totalSteps = int(self.stop/self.step)
        self.alpha = np.random.normal(0, 1.0, self.totalSteps)
        # Trajectory of protein when its # goes from 0 to steady state value is not
        # so exciting, let's chop it off.
        self.cutoff = int(30.0 / self.step)
        print("[INFO] cutting-off at {}".format(self.cutoff))
        print("|- Producing {} trajectories".format(ntimes))

        # Let's calculate n trajectories of solution,
        n = ntimes
        for i in range(n):
            output = self.solveLangevian()
            self.trajectories.append(output[self.cutoff:])
            print("|- mean: {}, std: {}".format(np.mean(output), np.std(output)))
            if len(self.stateBTimes) > 0:
                self.timeToCrossStateBFirstTime.append(self.stateBTimes[0])
            self.reinit()
        return self.trajectories

    def plotTrajectories(self, nos = -1, save = True):
        """Plot total number of trajectories """
        if nos == -1: nos = len(self.trajectories)
        plt.figure()
        for trajectory in self.trajectories[:nos]:
            x = [ (self.cutoff + i) * self.step for i in range(len(trajectory)) ]
            plt.plot(x, trajectory, linewidth=0.04)
        plt.xlabel("Time in seconds")
        plt.ylabel("x (protein number)")
        plt.title("protein: Using Langevian for k1k2={}, sim time = {}".format(
            self.k1k2, self.stop)
            )
        if save:
            plt.savefig('final_pics/langevin_trajectories_{:.0e}.png'.format(self.k1k2))
        else:
            plt.show()

    def plotHistogram(self, save = False):
        """Plot a histogram """
        plt.figure()
        collectedOutput = np.array([])
        for trajectory in self.trajectories:
            collectedOutput = np.append(collectedOutput, trajectory)
        hist, bins = np.histogram(collectedOutput, bins=100)
        plt.bar(bins[:-1], hist)
        plt.xlabel("#protein (x)")
        plt.ylabel("No of times x protein is seen")
        plt.title(
                "Distribution of #protein for k1k2={}, simulation time={} sec".format(
                    self.k1k2, self.stop
                    )
                )
        if save:
            plt.savefig('final_pics/distribution_{:.0e}.png'.format(self.k1k2))
        else:
            plt.show()

    def transitions(self, step, noOfTransitions, thresholdA, thresholdB):
        self.step = step
        self.alpha = np.random.normal(0, 1.0,1e7)
        self.stateAThreshold = thresholdA
        self.stateBThreshold = thresholdB 
        self.solveLangevianForTransitions(noOfTransitions)
        print("Done")

def main(problem = 1):
    gs = GeneticSwitch(k1k2=1e-4, init=0)
    print("++ Solving problem no {}".format(problem))
    if problem == 1:
        gs.run(step = 0.01, stop = 10000, ntimes = 1)
        gs.plotTrajectories(save = True)
        gs.plotHistogram( save = True)
    elif problem == 3:
        gs.transitions(step = 0.01, noOfTransitions = 100, thresholdA = 20,
                thresholdB = 125)

if __name__ == '__main__':
    main(3)
