import PyDSTool
from pylab import plot, show, linspace, xlabel, ylabel

# we must give a name
DSargs = PyDSTool.args(name='Calcium')
# parameters
DSargs.pars = { 'vl': -60,
               'vca': 120,
                 'i': 0,
                'gl': 2,
               'gca': 4,
                 'c': 20,
                'v1': -1.2,
                'v2': 18  }
# auxiliary helper function(s) 
DSargs.fnspecs  = {'minf': (['v'], '0.5 * (1 + tanh( (v-v1)/v2 ))') }
# rhs of the differential equation, including dummy variable w
DSargs.varspecs = {'v': '( i + gl * (vl - v) - gca * minf(v) * (v-vca) )/c',
                   'w': 'v-w' }
# initial conditions
DSargs.ics      = {'v': 0, 'w': 0 }


DSargs.tdomain = [0,40]                             # set the range of integration.
ode  = PyDSTool.Generator.Vode_ODEsystem(DSargs)    # an instance of the 'Generator' class.
traj = ode.compute('polarization')                  # 
pd   = traj.sample()                                # Data for plotting
plot(pd['t'], pd['v'])
xlabel('time')                                      # Axes labels
ylabel('voltage')                                   # ...
show()
