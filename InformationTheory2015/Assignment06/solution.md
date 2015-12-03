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
choices are $6^n$.

