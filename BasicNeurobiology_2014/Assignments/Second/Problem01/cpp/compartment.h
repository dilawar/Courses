/*
 * =====================================================================================
 *
 *       Filename:  compartment.h
 *
 *    Description:  Class for making a compartment
 *
 *        Version:  1.0
 *        Created:  Monday 10 February 2014 09:16:48  IST
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Dilawar Singh (), dilawar@ee.iitb.ac.in
 *   Organization:  
 *
 * =====================================================================================
 */


#ifndef  COMPARTMENT_INC
#define  COMPARTMENT_INC

#include <cmath>
#include <cassert>
#include <ginac/ginac.h>
#include <iostream>

/*
 * =====================================================================================
 *        Class:  Compartment
 *  Description:  
 * =====================================================================================
 */

using namespace std;
using namespace GiNaC;

class Compartment
{
    public:
        /* constructor      */
        Compartment ()
        {
            initSybols();
        }

        Compartment (double length)
        {
            initSybols();
            surfaceArea();
            _surfaceArea = _surfaceArea.subs(_length == length);

            crossSectionArea();
            _area = _area.subs(_length == length);

            equation(length);
        }

        void initSybols(void)
        {
            _RM = 1.0;
            _RA = 1.0;
            _radius.set_name("r");
            _length.set_name("l");
            v1.set_name("V1");
            v2.set_name("V2");
        }


        /* destructor       */
        ~Compartment ()
        {

        }

        ex& getEquation(void)
        {
            return _equation;
        }

        void crossSectionArea()
        {
            _area = M_PI * pow(_radius, 2);
        }

        void surfaceArea()
        {
            _surfaceArea = 2 * M_PI * _radius * _length;
        }

        /* Return the equation of compartment */
        ex& equation(double length)
        {
            ex ra = (_RA * _length) / _area;
            ra = ra.subs(_length == length);

            ex rm = _RM / _surfaceArea;
            rm = rm.subs(_length == length);

            _equation = ((ra / (ra + rm)) * v1) - v2;

            return _equation;
        }

    public:
        double _RM;
        double _RA;

        symbol _radius; //("radius");
        symbol _length; //("length");

        ex _area;
        ex _surfaceArea;
        ex _equation;

        symbol v1; //("v1");
        symbol v2; //("v2");

}; /* -----  end of class Compartment  ----- */

#endif   /* ----- #ifndef COMPARTMENT_INC  ----- */
