# This script generates Morse code and apply Shannon's idea.

import random
import math
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

def entropy_seq( seq ):
    probs = {}
    entropy = 0.0
    for s in symbols:
        p = seq.count(s) / float(len(seq))
        if p == 0.0:
            continue
        entropy += ( - p * math.log(p, 2))
    return entropy

def random_step(

def generate_seq( n ):
    # Select a random node from graph as starting point.
    start = random.choice(state_.nodes())
    seq = [ start ]
    while len(seq) < n:
        n = random_step(seq[-1])
        print n
        quit()

def main():
    seq = generate_seq( n = 10 )

if __name__ == '__main__':
    main()
