# This script generates Morse code and apply Shannon's idea.

import random
import math

symbols = [ ".", "-", "L", "W" ]

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
        entropy += ( - p * math.log(p, 2))
    return entropy

def generate_seq( n ):
    seq = [ random.choice(symbols) ]
    for i in range(n-1):
        seq.append(transmitter(seq[-1]))
    return seq

def main():
    morse_seq = generate_seq(100000)
    print entropy_seq(morse_seq)

if __name__ == '__main__':
    main()
