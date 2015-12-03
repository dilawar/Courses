import os
import sys
import numpy as np

def twos_are_twice_of_ones(n):
    dice = np.random.random_integers(1, 6, n)
    ones = np.where(dice == 1)[0]
    twos = np.where(dice == 2)[0]
    print('2s=%s, 1s=%s' % (len(twos), len(ones)))
    if len(twos) >= 2 * len(ones):
        return True
    return False

def simulate():
    found = False
    i = 0
    while(not found):
        i += 1
        if twos_are_twice_of_ones( 10000000 ):
            found = True
            print("Found after %s attempts" % i)

def main():
    length = 10
    alphas = range(1, 7)
    print alphas

def distribute(n, p):
    # Distribute n different elements in p places.



if __name__ == '__main__':
    test()
