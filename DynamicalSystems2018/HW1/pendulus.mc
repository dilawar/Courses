/* Pendulum with non-linearlity */
load( contrib_ode )$

/* This was derived in class */
/* declare( [m, L, g, theta], real)$ */
pendulum : m * L^2 * 'diff(theta,t,2) + b * 'diff( theta, t ) 
    + m * g * L * sin(theta) = gamma$

/* pendulum : expand( pendulum / m / L / L ); */
/* Subtiture w = root(g/L) and tau = wt */
/* pendulum : subst( w^2 * L, g, pendulum ); */

/* contrib_ode( pendulum, theta, t ); */

/*************************************************************
 * This second order system can be written as system of ODES
 **************************************************************/
eq1 : 'diff(theta,t) = u;
eq2 : 'diff(u,t) = ( -b * u - m*g*L*sin(theta) + gamma ) / m / L / L;

