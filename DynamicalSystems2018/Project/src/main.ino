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

#define NUM_RING        6
#define NUM_LED_IN_RING 6
#define NUM_LED         36

int latchPin = 6;  // Connected to ST_CP (pin 12) of 74HC595
int clockPin = 5;  // Connected to SH_CP (pin 11) of 74HC595
int dataPin  = 7;  // Connected to DS (pin 14) of 74HC595
// MAKE SURE YOU CHANGE THESE TO SUIT WHICH PINS YOU HAVE USED!


int row;
int col;

byte Byte[5] ={};

size_t ring_ = 0;
size_t led_  = 0;

bool status_[36] = {0};
long last_active_[36] = {0};

void setup()
{

  Serial.begin(38400);  
  
  pinMode(latchPin, OUTPUT);
  pinMode(clockPin, OUTPUT);
  pinMode(dataPin, OUTPUT);

  digitalWrite(latchPin, HIGH);
  digitalWrite(clockPin, HIGH); 
 
}

void LedON(int row, int col)
{

    row--;
    col--;

    Byte[0] = (Byte[0] | LED[row][col][0]);
    Byte[1] = (Byte[1] | LED[row][col][1]);
    Byte[2] = (Byte[2] | LED[row][col][2]);
    Byte[3] = (Byte[3] | LED[row][col][3]);
    Byte[4] = (Byte[4] | LED[row][col][4]);

    for(int i=0 ; i<5 ;i++)
    {
        digitalWrite(latchPin, LOW);      
        shiftOut(dataPin, clockPin, MSBFIRST,  Byte[i]);                   
        digitalWrite(latchPin, HIGH); 
    }    
}


void LedOFF(int row, int col)
{

    row--;
    col--;

    Byte[0] = (Byte[0] & ~LED[row][col][0]);
    Byte[1] = (Byte[1] & ~LED[row][col][1]);
    Byte[2] = (Byte[2] & ~LED[row][col][2]);
    Byte[3] = (Byte[3] & ~LED[row][col][3]);
    Byte[4] = (Byte[4] & ~LED[row][col][4]);

    for(int i=0 ; i<5 ;i++)
    {
        digitalWrite(latchPin, LOW);      
        shiftOut(dataPin, clockPin, MSBFIRST,  Byte[i]);                   
        digitalWrite(latchPin, HIGH); 
    }       

    status_[row*NUM_RING+col] = false;
}

void ledON( size_t index )
{
    ring_ = index / NUM_RING  + 1;
    led_ = ( index % NUM_LED_IN_RING ) + 1;
    LedON( ring_, led_ );
    last_active_[index] = millis();
    status_[index] = true;
}

void ledOFF( size_t index )
{
    ring_ = index / NUM_RING  + 1;
    led_ = ( index % NUM_LED_IN_RING ) + 1;
    LedOFF( ring_, led_ );
    status_[index] = false;
    last_active_[index] = millis();
}

/* --------------------------------------------------------------------------*/
/**
 * @Synopsis  Led with given index.
 *
 * @Param index
 */
/* ----------------------------------------------------------------------------*/
void update( size_t index )
{
    // Check if led is ON. If it is ON, switch it OFF.
    if( status_[index] )
    {
        // Serial.print( "Index is on ");
        // Serial.println( index );

        if( (last_active_[ index ] - millis()) > 1000 )
            ledOFF(index);
    }
    else
        ledON( index );

}


void loop()
{ 
    for (size_t i = 0; i < NUM_LED_IN_RING * NUM_RING; i++) 
        update( i );

    delay( 200 );
}
