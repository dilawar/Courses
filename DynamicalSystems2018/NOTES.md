---
title : Notes on Dynamical Systems 
author : Dilawar Singh
date : \today
---

# Definitions

## Conservative Systems

1. If a dynamical system $\dot{x}=f(x),\; x\in\mathbb{R}$ Jacobian $D_xf(x)$ has
   trace 0 then it is conservative. The sum of eigenvalues is zero. Implies,
   there would not be any sink or source in conservative system.

- Conservative systems preserve phase space volume. __??__


# Solutions of ODEs

$y' = f(x,y), y(x_0)=y_0$

__Existence of solution__  If function f is continuous in some region R
containing the point (x~0~,y~0~) then there exists at least one  solution in R.
Note that f is necessarily bounded in R.

__Uniqueness of solution__ If derivative of f with respect to y
($\frac{df}{dy}$) is also continuous in R (same as before) then the solution is
unique in R' where R' is contained in R.

# Index of Trajectories

Assumption __There is no fixed point on the trajectory C.__

The __Index__ of trajectory C ($I_C$) is equal to the number of fixed points
inside the trajectory. The sign is negative if trajectory is counter-clockwise.

## Dulac's criteria

$\exists g$ s.t. iff $\int_c g.x$ has same sign then $x$ is not periodic.

It follows from 'Green Theorem'.

# Birfuracation

## Hopf's bifurcation

When complex conjugate eigen values cross the imaginary axis from the negative real axis ,
we see a Hopf bifurcation.

- Supercritical Hopf (Stable)
- Subcritical Hopf (Unstable)
- Homoclinic bifurcation
