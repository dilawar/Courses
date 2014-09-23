
"""election_poisson.py: 

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


import os
import sys
from collections import Counter
from scipy.stats import poisson, ks_2samp
import csv

votes = {}
totalVotes = Counter()
winner = {}
winnerShare = Counter()

def buildDB(filename):
    f = open(filename, 'r') 
    incsv = csv.reader(f, delimiter=',', quotechar='"')
    incsv.next()
    for l in incsv:
        constituency = l[1]
        candidate = l[2]
        votes[candidate] = int(l[3])
        totalVotes[constituency] += votes[candidate]
        if l[4] == "yes":
            winner[constituency] = candidate 

def processWinners(winners):
    share = []
    for w in winners:
        winner = winners[w]
        v = votes[winner]
        #print("{} won. votes {} outof {}".format(winner, v, totalVotes[w]))
        share.append(100 * float(v) / totalVotes[w])
    newShare = []
    for x in share:
        if int(x) > 0:
            newShare.append(5 * int(x/5.0))
    winnerShare = Counter(newShare)
    plot(winnerShare)

def plot(counter):
    print("[INFO] Plotting counter")
    import pylab
    x = list(counter)
    y = counter.values()
    totalY = sum(y)
    y = [ float(r) / totalY for r in y ]
    #assert sum(y) == 1.0, "Not equalt to 1.0 != {}".format(sum(y))
    # mean and std-deviation
    fig, ax = pylab.subplots()
    ax.bar(x, y, color='y')
    poss = poisson.pmf(x, 45)
    factor = 1.0 / sum(poss)
    ax.plot(x, 0.7 * factor * poss, 'o')
    print ks_2samp(poss, y)
    pylab.savefig('election_poisson.png')

def main():
    filename = sys.argv[1]
    buildDB(filename)
    processWinners(winner)

if __name__ == '__main__':
    main()
