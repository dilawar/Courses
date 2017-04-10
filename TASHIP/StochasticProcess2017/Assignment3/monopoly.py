import sys
import os
import numpy
import random

class Board():
    """This class simulates the given Monopoly"""
    def __init__(self):
        self.tokenPos = 0
        self.totalMoves = 0
        self.history = []

    def moveNext(self, m):
        newPos = ( self.tokenPos + m ) % 40
        # print("+ Moving to {}".format(newPos))
        if newPos == 29:
            print("||| Jailed")
            self.tokenPos = 9
            # self.history.append( 29 )
        else:
            self.tokenPos = newPos 
        self.history.append(self.tokenPos)

    def rollDiceAndMove(self):
        step = random.randint(2, 12)
        self.moveNext(step)

class Markov():
    """This class uses Markov matrix to answer the questions """
    def __init__(self):
        self.matrix = numpy.zeros(shape=(40,40))
        self.markov = numpy.zeros(shape=(39,39))

    def prob(self, roll):
        """Probability of getting index when two dice are thrown."""
        if roll == 0:
            return 0.0
        elif roll <= 7:
            return (roll - 1) / 36.0
        else:
            return self.prob(14-roll)
        
    def getRow(self, index):
        row = numpy.zeros(shape=40)
        start = index
        stop = index + 12
        for i in range(stop - start + 1):
            row[(start+i)%40] = self.prob(i)
            # What if we land is jail from the start position.
            if start + i == 29:
                print("++ I can go to jail from here")
                row[9] = row[29]
                row[29] = 0.0
        assert abs(sum(row) - 1.0) < 1e-10, "Prob sum is not 1.0, %s" % sum(row)
        return row

    def getEigens(self, matrix):
        e, v = numpy.linalg.eig(matrix)
        maxe = 0
        index = 0
        for i, a in enumerate(e):
            if abs(a) > maxe:
                maxe = a
                index = i
        return e[index], v[:,index]

    def verify(self):
        for row in self.markov:
            assert abs(sum(row) - 1.0) < 1e-10, "Not a valid row."

    def initMatrix(self):
        for i in range(40):
            self.matrix[i] = self.getRow(i)
            if i == 29: 
                # This is go to jail row.
                print("++ Row 29 i.e. go to jail should not have any probs.")
                self.matrix[i] = numpy.zeros(shape=40)
        #self.matrix = self.matrix.transpose()
        # Markovian matrix should not have row 29 and column 29.
        a  = numpy.delete(self.matrix, 29, 0)
        a = numpy.delete(a, 29, 1)
        self.markov = a
        self.verify()
        numpy.savetxt('transition.out', self.matrix, delimiter=',', fmt='%1.2f')
        numpy.savetxt('markov.out', self.markov, delimiter=',', fmt='%1.2f')

    def run(self):
        self.initMatrix()
        e1, v1 = self.getEigens(self.markov)
        print e1
        print v1
        pow = 50
        print("Raising the power to %s" % pow)
        b = numpy.linalg.matrix_power(self.markov, pow)
        numpy.savetxt('markov_%s.out' % pow, b, delimiter=',', fmt='%1.3f')
        e2, v2 = self.getEigens(b)
        print e2
        print v2
        print("Diff between max eigenvalues: {}".format(abs(e2) - abs(e1)))
        print("Diff between eigenvectors: {}".format(e2 - e1))
        self.valueCheck(b)

    def valueCheck(self, mat):
        values =  {1:60, 3:60,4:-200,5:200,6:100,8:100,9:120,11:140,12:150
                ,13:140,14:160,15:200,16:180,18:180,19:200,21:220,23:220
                ,24:240,25:200,26:260,27:260,28:150,29:280,31:300,32:300
                ,34:320,35:200,37:350,38:100,39:400
                }
        sorted = numpy.argsort(mat, axis=1)
        numpy.savetxt('sorted.out', sorted, delimiter=' ', fmt='%2.0f')
        newvalues = dict()
        for i, p in enumerate(sorted[1]):
            if values.get(p, None) is not None:
                newvalues[p] = float(mat[0][p] * values[p])
        print newvalues

def play(step):
    board = Board()
    for s in range(1,step):
        board.rollDiceAndMove()
    return board.history

def simulate():
    import pylab
    from collections import Counter
    N = 10**6
    moves = play( N )
    counter = Counter(moves)
    xvals = list(counter)
    yvals = [ float(x) / sum(counter.values()) for x in counter.values()]
    pylab.subplot(211)
    pylab.bar(xvals, yvals)
    pylab.ylabel( "Values" )
    pylab.xlabel( "Location index e.g. 29 is JAIL" )
    pylab.title("Played %d times" % N )
    pylab.subplot( 212 )
    pylab.hist( moves, bins = max( moves ) + 1, normed = True )
    pylab.xlabel( "Location index e.g. 29 is JAIL" )
    pylab.ylabel( 'Frequency of visit' )
    pylab.tight_layout( )
    pylab.savefig('monopoly_%s.png' % sys.argv[1] )
    # pylab.show()
    
if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("USAGE: {} play|markov".format(sys.argv[0]))
        sys.exit()
    if sys.argv[1] == 'play':
        simulate()
    elif sys.argv[1] == 'markov':
        a = Markov()
        a.run()
    else:
        print("USAGE: {} play|markov".format(sys.argv[0]))
        sys.exit()
