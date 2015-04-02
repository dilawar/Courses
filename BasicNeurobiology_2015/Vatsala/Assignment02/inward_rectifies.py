import numpy as np
import pylab

def channel(vm):
    time = np.arange(0, 150e-3, 1e-3)
    tau1, tau2, tau3 = 1e-3, 10e-3, 10000e-3
    c = -vm/10e-3
    c = (abs(c) + c)/2
    print c
    c1 = c**0.2
    current = np.zeros(len(time))
    for i, t in enumerate(time):
        current[i] = c * np.exp(-t/tau1) - c1*np.exp(-t/tau2) - c*np.exp(-t/tau3)
        current[i] += np.random.random()/50
    pylab.plot(time, current, label="%s V"%vm)
    pylab.legend()
    return  time, current


def main():
    channel(-80e-3)
    channel(-30e-3)
    channel(0e-3)
    channel(30e-3)
    pylab.title("Inward rectifying K-current")
    pylab.savefig("inward_rectifying.png")


if __name__ == '__main__':
    main()
