# Problem two

import numpy as np
import math

def compute_entropy(array):
    array = filter(lambda x: x != 0.0, array)
    return sum([-p * math.log(p, 2.0) for p in array ])

class JointDist():

    def __init__(self, ncoins):
        print("[INFO] Total coins: %s" % ncoins)
        self.prob_x = [ 1/2.0, 1/4.0, 1/4.0 ]
        self.states = 2*ncoins +1
        self.mat = np.zeros((self.states, 3), dtype=np.float)
        self.init_dist()

    def init_dist(self):
        print("[INFO] Intializing distribution")
        for i, row in enumerate(self.mat):
            for j, v in enumerate(row):
                self.mat[i, j] = 1/25.0
        self.rescale_dist()

    def rescale_dist(self):
        # Rescale 
        for i, col in enumerate(self.mat.T):
            scale = self.prob_x[i] / sum(col)
            self.mat[:,i] = col * scale
        assert np.allclose(np.sum(self.mat), 1.0)

    def compute_hx(self):
        return compute_entropy(self.prob_x)

    def compute_hy(self):
        ys = []
        for row in self.mat:
            ys.append(sum(row))
        assert np.allclose(sum(ys), 1.0)
        return compute_entropy(ys)

    def compute_hxy(self):
        assert np.allclose(np.sum(self.mat), 1.0)
        entropy = 0.0
        for row in self.mat:
            entropy += compute_entropy(row)
        return entropy

    def mutual_info(self):
        hx = self.compute_hx()
        print("hx = %s" % hx)
        hy = self.compute_hy()
        print("hy = %s" % hy)
        hxy = self.compute_hxy()
        print("hxy = %s" % hxy)
        return hx + hy - hxy

    def balance(self, xcoins):
        """Put x coin on 1 side """
        weightedCoins = 2*xcoins
        for i, col in enumerate(self.mat.T):
            newcol = []
            for j, v in enumerate(col): 
                if j < weightedCoins:
                    if i < 2:
                        newcol.append(1.0/(2**xcoins))
                    else:
                        newcol.append(0.0)
                else: 
                    if i < 2:
                        newcol.append(0.0)
                    else:
                        newcol.append(1.0/(self.states-(weightedCoins)))
            self.mat[:,i] = newcol
        self.rescale_dist()

def main(ncoins):
    dist = JointDist(ncoins)
    for j in range(0, 6):
        print("Weigh %s coin on both side" % (j+1))
        print dist.mat
        dist.balance(j+1)
        i = dist.mutual_info()
        print("Mutual info: %s" % i)

if __name__ == '__main__':
    ncoins = 12
    dist = JointDist(ncoins)
    main(ncoins)
