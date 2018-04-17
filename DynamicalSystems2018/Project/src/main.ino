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
    Serial.begin( 38400 );

    pinMode(latchPin, OUTPUT);
    pinMode(clockPin, OUTPUT);
    pinMode(dataPin, OUTPUT);

    digitalWrite(latchPin, HIGH);
    digitalWrite(clockPin, HIGH); 
}

void writeData( byte val )
{
    digitalWrite( latchPin, LOW );
    shiftOut( dataPin, clockPin, MSBFIRST, val );
    digitalWrite( latchPin, HIGH );
}

void turnON( size_t groupnum, size_t lednum, size_t wait = 100 )
{

    String msg( "LED " );
    msg += String( groupnum );
    msg += String( lednum );

    for (size_t i = 0; i < 5; i++) 
    {
        byte val = LED[groupnum][lednum][i];
        writeData( val );
        msg += String( " " );
        msg += String( val ); 
    }
    Serial.println( msg );
    delay( wait );
}

void showLEDS( size_t wait = 100 )
{
    for (size_t i = 0; i < 6; i++) 
        for (size_t ii = 0; ii < 6; ii++) 
            turnON( i, ii, wait );
}


void testLEDSetup()
{

#if 1
    // writeData( random(0,129) );
    writeData( 0x20 );
    delay( 500 );
#else
    showLEDS( 500 );
#endif
}

void loop()
{ 
    testLEDSetup();
    delay(100);
}
