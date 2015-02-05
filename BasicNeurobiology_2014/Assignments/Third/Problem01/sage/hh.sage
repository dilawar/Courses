t = var('t')
Cm, gl, El, gNa, ENa, gK, Ek, I = var('Cm gl El gNa ENa gK Ek I')
V = function('V',t)
m = function('m', t)
n = function('n', t)
h = function('h', t)

eq =  diff(V, t) == (1 / Cm) * (- gl * ( V - El) - gNa * (m^3) * h * ( V - ENa) \
        - gK * (n^4) * (V - Ek))
eq = eq.subs(Cm == 1.0e-2)
eq = eq.subs(ENa == 50e-3)
eq = eq.subs(Ek == -77e-3)
eq = eq.subs(El == -54.4e-3)
eq = eq.subs(gK == 36e+1)
eq = eq.subs(gNa == 120e+1)
eq = eq.subs(gl == 0.3e+1)

# Sodium activation and inactivation
#am = function('am', V)
#bm = function('bm', V)
am = 0.1 * (V + 40e-3) / ( 1 - exp(-(V + 40e-3)/10))
bm = 4 * exp(-(V + 65e-3)/18e-3)
eq_K = diff(m, t) == am * (1 - m) - bm * m

# Potassium
#an = function('an', V)
#bn = function('bn', V)
an = 0.01 * (V + 55e-3) / ( 1 - exp(-(V + 55e-3)/10))
bn = 0.125 * exp(-(V + 65e-3)/80e-3)
eq_Na = diff(n, t) == an * (1 - n ) + bn * n 

# h
ah = function('ah', V)
bh = function('bh', V)
eq_h = diff(h, t) == ah * (1 - h) - bh * h
print eq
print eq_Na
print eq_K
print eq_h


system = [eq, eq_Na, eq_K, eq_h]
vars = [V, n, m, h]
ics=[-60e-3, 2, 0.02, 5]
times = srange(0, 6, 0.001)
sympy=False
if not sympy:
    sol = desolve_system(system, vars, ics,ivar=t)
else:
    sol = desolve_odeint( [x.right_hand_side() for x in system]
            , ics
            , times
            , vars
            , ivar=t
            , rtol=1e-13
            , atol=1e-15
            )
    #p = line(zip(sol[:0], sol[:1]))
    #p.show()
