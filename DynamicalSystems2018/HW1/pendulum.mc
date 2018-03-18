/* Solve ODE and plot trajectories */
load( plotdf )$

eq1 : 'diff(theta,t) = omega;
eq2 : 'diff(omega,t)= alpha * omega + I - sin( theta );

/* generate phase plot and solutions */
plotdf( [ rhs(eq1), rhs(eq2) ]
        , [ theta, omega ]
        , [ theta, -2*%pi, 2*%pi ]
        , [ omega, -2*%pi, 2*%pi ]
        , [parameters, "I=0,alpha=0"]
        , [tstep,0.01]
        , [trajectory_at, 3.14, 0]
        , [nsteps,500]
        , [sliders,"I=0:1,alpha=-1:1"]
        )$

