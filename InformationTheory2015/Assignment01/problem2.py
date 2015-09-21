import numpy as np
import random

class Coin():
    def __init__(self, id):
        self.weight = 0
        self.id = id
    def __ge__(self, ob):
        return self.weight > ob.weight

    def __gt__(self, obj):
        return self.weight >= obj.

def coins_with_one_counterfiet(n):
    coins = [ Coin(x) for x in range(n-1) ]
    # Lighter of heavier coin
    c = Coin(n)
    c.weight = random.choice([-1,1])
    coins.append(c)
    random.shuffle(coins)
    return coins

def main():
    coins = coins_with_one_counterfiet(12)
    print coins

if __name__ == '__main__':
    main()
