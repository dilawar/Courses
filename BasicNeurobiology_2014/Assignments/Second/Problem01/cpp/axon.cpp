/*
 * =====================================================================================
 *
 *       Filename:  problem.cpp
 *
 *    Description:  This program solves the problem after converting the axon in
 *    1000 compartments.
 *
 *        Version:  1.0
 *        Created:  Monday 10 February 2014 09:03:48  IST
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Dilawar Singh (), dilawar@ee.iitb.ac.in
 *   Organization:  
 *
 * =====================================================================================
 */
#include <stdlib.h>
#include <iostream>
#include <ginac/ginac.h>
#include "compartment.h"

using namespace std;
using namespace GiNaC;

int main()
{
    double dx = 0.05;
    Compartment c = Compartment(dx);
    ex& eq = c.getEquation();
    eq = eq.subs(lst(c.v1 == 0.060, c.v2 == 0.038));
    cerr << "And the equation is " << eq.normal() << endl;
    double wolfram = 0.0029847;
    cerr << " Puting wolfram answer back " << eq.subs(c._radius == wolfram) << endl;
    cerr << "Solution : " << fsolve(eq, c._radius, 0, 30);
    return 0;
}

