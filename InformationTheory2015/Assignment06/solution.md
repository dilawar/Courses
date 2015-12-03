---
title: Assignment 6
author: Dilawar Singh
date : \today 
header-includes:
    - \usepackage[margin=15mm]{geometry}
---

# Problem 1 Large deviation theory. 
We roll a fair six-sided die $n$ times, where $n$ large. What is the probability
that the number of 2's is at least double the number of 1s in the resulting
string? 

Of strings which satisfy this condition, what proportion of values will be 1s?  

_In a simulation with n = $10^7$  ran for few hours on a server, we never saw no
of 2s ever becoming larger than twice the numbers of 1s. In fact, in most cases,
they were almost equal ($10^7/6$)._

## a

Each alphabet i.e. $(1,2,\ldots,6)$ can take $n$ possible values. Total
choices are $6^n$. In the following cases, no of 2s are at least twice the
number of 1s.

Following types satisfies the constraints:

    Type                No of elements in typeclass

    (0, 1/n, .... )     (n-1)(n-2)(n-3)(n-4)
    (0, 2/n, ....)      (n-2)(n-3)(n-4)(n-5)
    (0, 3/n, ....)      (n-3)(n-4)(n-5)(n-6)

    ...
    (1/n,2/n, ...)      (n-3)(n-4)(n-5)(n-6)
    (1/n,3/n, ...)      (n-4)(n-5)(n-6)(n-7)
    ...
    ...

    (p/n, 2p/n, ...)    (n-3p)(n-3p-1)(n-3p-2)(n-3p-4)
    (p/n, 2p+1/n, ...)  (n-3p-1)(n-3p-2)(n-3p-3)(n-3p-5)
    ... total (n-p) times


    (1/3, 2/3, ....)    1
    and so on..

Effectively
    
$$ |E| < n(n-1)(n-2)(n-3)(n-4) + (n-1)(n-2)(n-3)(n-4)(n-5) + (n-2)(n-3) \ldots + 1  $$

## b


