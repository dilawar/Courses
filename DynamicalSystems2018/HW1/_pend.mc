/* Solve ODE and plot trajectories */
load( plotdf )$
load( draw )$

eq1 : 'diff(theta,t) = omega;
eq2 : 'diff(omega,t)= alpha * omega + I - sin( theta );

/* generate phase plot and solutions */
/* I = 0.0, 0.1, 0.5, 1.0
   alpha = 0.0, 0.1, 0.05, 1.0
*/
PI: 3.1416;
I : 0.1;
alpha : 0.01;

traj : map( 
    lambda([x], rk([rhs(eq1), rhs(eq2)], [theta, omega]
    , [random( 4*PI) - 2*PI, random( 4*PI) - 2*PI], [t, 0, 30, 0.1]))
    , makelist(1,50)
    )$

trajs : map( lambda( [t], map( lambda( [x], [ second(x), third(x) ] ), t ) ), traj )$
plots : map( lambda( [p], points(p) ), trajs )$

plot1: draw2d( points_joined=true, point_size=0.2, point_type=circle, plots )$
draw_file( terminal = png, file_name = "./a" )$
