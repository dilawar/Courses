-- Chapter 1, Problem 1

import GSL.Random.Dist

rate = 100
t = 1
dist = map (\x -> poissonPdf x 1.0) [1..(rate*t)]
