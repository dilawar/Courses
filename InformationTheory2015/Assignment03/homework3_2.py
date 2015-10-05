# This script generates Morse code and apply Shannon's idea.

import random
import math
import numpy as np
import matplotlib.pyplot as plt
import pygraphviz as pvg

state_ = pvg.AGraph("./state.dot", directed=True)

def transmitter( prev = None ):
    """Transmit one of the symbols.
    The generated symbol depends on the previous symbol prev.
    """
    if not prev:
        return random.choice(symbols)
    if prev == 'L' or prev == "W":
        return random.choice([".", "-"])
    else:
        return random.choice(symbols)

def random_step(node):
    ## Take a random step now.
    outN = state_.out_neighbors(node)
    probs = []
    for n in outN:
        e = state_.get_edge(node, n)
        probs.append(float(e.attr['p']))
    return np.random.choice(outN, 1, p=probs)[-1]

def entropy_seq( seq ):
    probs = {}
    entropy = 0.0
    for s in symbols:
        p = seq.count(s) / float(len(seq))
        if p == 0.0:
            continue
        entropy += ( - p * math.log(p, 2))
    return entropy

def generate_seq( n ):
    # Select a random node from graph as starting point.
    print("generating %s" % n)
    start = random.choice(state_.nodes())
    seq = [ start ]
    while len(seq) < n:
        x = random_step(seq[-1])
        seq.append(x)
    return seq

def main():
    seq = generate_seq( n = 10**5 )

if __name__ == '__main__':
    main()
