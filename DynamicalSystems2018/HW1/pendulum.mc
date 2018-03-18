/* Solve ODE and plot trajectories */
load( plotdf )$

eq1 : 'diff(theta,t) = omega;
eq2 : 'diff(omega,t)= alpha * omega + I - sin( theta );

/* generate phase plot and solutions */
plotdf( [ rhs(eq1), rhs(eq2) ]
        , [ theta, omega ]
        , [ theta, -4*%pi, 4*%pi ]
        , [ omega, -4*%pi, 4*%pi ]
        , [parameters, "I=0.5,alpha=0.1"]
        , [tstep,0.1]
        , [trajectory_at, 0, 0]
        , [nsteps,500]
        , [sliders,"I=0:2,alpha=0:1"]
        )$

