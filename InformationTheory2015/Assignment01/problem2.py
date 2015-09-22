import numpy as np
import random
import networkx as nx
import matplotlib.pyplot as plt

class Coin():
    def __init__(self, id):
        self.weight = 0
        self.id = id

def coins_with_one_counterfiet(n):
    coins = [ Coin(x) for x in range(n-1) ]
    # Lighter of heavier coin
    c = Coin(n)
    c.weight = random.choice([-1,1])
    coins.append(c)
    random.shuffle(coins)
    return coins

def weight(coins):
    return sum([x.weight for x in coins ])

def split_coins(coins):
    """
    Split the coins into two equal parts. 

    If the coins are odd numbers, then leave one coins. and check the rest.
    """
    extra = None
    if len(coins) % 2 == 0: 
        extra = None
        left, right = coins[0:len(coins)/2], coins[len(coins)/2:]
    else: 
        extra = coins[-1]
        left, right = coins[0:len(coins)/2], coins[len(coins)/2:-1]
    return left, right, extra

def draw_graph(graph, plot = True, outfile = None):
    nx.draw(graph)
    if plot:
        plt.show()
    elif outfile:
        print("[INFO] Saving files to %s" % outfile)
        plt.savefig(outfile)

def branch(graph, node_id):
    """Create a computational branch and return the nodes to execute the next
    step of algorithm
    """
    coins = graph.node[node_id]['coins']
    left, right, extra = split_coins(coins)
    if extra is None:
        graph.add_node(node_id + 1, coins = left)
        graph.add_edge(node_id, node_id + 1)
        graph.add_node(node_id + 2, coins = right)
        graph.add_edge(node_id, node_id + 2)

def solve(graph, node_id):
    """Takes a graph with first node as all coins. Recurse over it to solve the
    graph"""
    n = branch(graph, node_id)
    draw_graph(graph)


def main():
    coins = coins_with_one_counterfiet(12)
    coinGraph = nx.Graph()
    coinGraph.add_node(0, coins = coins, weight = weight(coins))
    solve(coinGraph, 0)

if __name__ == '__main__':
    main()
