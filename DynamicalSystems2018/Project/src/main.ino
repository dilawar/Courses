/***
 *       Filename:  main.ino
 *
 *    Description:  An implementation of Lorenz system in Arduino. 
 *         Author:  Dilawar Singh <dilawars@ncbs.res.in>
 *   Organization:  NCBS Bangalore
 *
 *        License:  MIT
 *
 *         CREDIT:  Hardware was defined by Dora Babu at electrical workshop of
 *                  NCBS Bangalore.
 */

#include "LEDADD.h"

int clockPin = 5;  // Connected to SH_CP (pin 11) of 74HC595
int latchPin = 6;  // Connected to ST_CP (pin 12) of 74HC595
int dataPin  = 7;  // Connected to DS (pin 14) of 74HC595

void setup()
{

    pinMode(latchPin, OUTPUT);
    pinMode(clockPin, OUTPUT);
    pinMode(dataPin, OUTPUT);

    digitalWrite(latchPin, HIGH);
    digitalWrite(clockPin, HIGH); 
}

void turnON( size_t groupnum, size_t lednum, size_t step = 5 )
{
    for (size_t i = 0; i < step; i++) 
    {
        digitalWrite( latchPin, LOW );
        shiftOut( dataPin, clockPin, MSBFIRST, LED[groupnum][lednum][i] );
        digitalWrite( latchPin, HIGH );
    }
}


void testLEDSetup()
{

    for(int k=0; k<6; k++)
    { 
        for(int j=0 ; j<6 ;j++)
        {
            turnON( k, j );
            delay(100);
        }
    }
}

void loop()
{ 
    testLEDSetup();
    delay(100);
}
