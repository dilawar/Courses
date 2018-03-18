/* Solve ODE and plot trajectories */
load( contrib_ode )$
load( draw )$
load( plotdf )$

keepfloat:true$
eq1 : 'diff(theta,t) = omega;
eq2 : 'diff(omega,t)= alpha * omega + I - sin( theta );

/* sol : contrib_ode( [eq1, eq2], [omega, theta], t ); */
/* plotdf( [ rhs(eq1), rhs(eq2) ], [ omega, theta ] ); */
 plotdf( [ rhs(eq1), rhs(eq2) ]
        , [ omega, theta ]
        , [parameters, "I=0,alpha=0"]
        , [tstep,0.01]
        /* [alpha,-1,1], [I,0,1] */
        , [nsteps,100]
        , [sliders,"I=0:1,alpha=-1:1"]
        )$


/*
subplot1: gr2d( implicit( first(icc),t,0,10,x,-5,5 ) 
    , grid = true
    , xlabel = "t", ylabel = "x" )$
subplot2: gr2d( xlabel = "x", ylabel = "dx"
    , explicit( rhs(eq), x, -2, 3) 
    , grid = true
    )$

draw( subplot1, subplot2
    , dimensions = [1000,500]
    , columns = 2
    , terminal = pdfcairo
    , file_name = "pendulum" 
    );
*/
