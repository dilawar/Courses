import numpy as np
import math

# State matrix.
state_mat = np.matrix([
            [1.0/4, 1.0/4, 1.0/4, 1.0/4]
            , [1.0/4, 1.0/4, 1.0/4, 1.0/4]
            , [1.0/2, 1/2.0, 0, 0 ]
            , [1/2.0, 1/2.0, 0 , 0]
            ], dtype=np.float).T

def print_mat(mat, label):
    print("%s:" % label)
    print(mat)
    print("W == %s\n" % mat.sum())

def entropy(arr):
    e = 0.0
    for x in arr:
        if x == 0.0:
            e += 0.0
        else:
            e += (-x * math.log(x, 2.0))
    return e

def entropy_rate(mat, probs):
    e = 0.0
    for i, p in enumerate(probs):
        e += p * entropy(mat[:,i])
    return e

def main():
    total_weight = np.sum(state_mat)
    print_mat(state_mat, "A")
    n = 1
    while (state_mat**n !=  state_mat**(n+1)).all():
        n += 1
    new_mat = state_mat**n
    print_mat(new_mat, "B")
    print("Entropy rate: %s" % entropy_rate(new_mat, [0.25,0.25,0.25,0.25]))

if __name__ == '__main__':
    main()
