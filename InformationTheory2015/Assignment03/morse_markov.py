import numpy as np
state_mat = np.matrix( [
            [1.0/4, 1.0/4, 1.0/4, 1.0/4]
            , [1.0/4, 1.0/4, 1.0/4, 1.0/4]
            , [1.0/2, 1/2.0, 0, 0 ]
            , [1/2.0, 1/2.0, 0 , 0]
            ])

def main():
    n = 1
    while (state_mat**n !=  state_mat**(n+1)).all():
        n += 1
    print n

if __name__ == '__main__':
    main()
