#include <stdio.h>
#include "nesdis.h"

//////////////////////////////////
/* Given a tag block, unblock it in to a short in array of 1s and 0s */
int unblock(short int *unblocked, block *tag) 
{
  int i, j, index ;
    unsigned char m1=128;
    unsigned char m2= 64;
    unsigned char m3= 32;
    unsigned char m4= 16;
    unsigned char m5=  8;
    unsigned char m6=  4;
    unsigned char m7=  2;
    unsigned char m8=  1;
  #ifdef FLIP
    #define WD1 0
    #define WD2 1  
  #else
    #define WD1 1
    #define WD2 0  
  #endif

  for (j = 0; j < 160; j++) {
    for (i = 0; i < 10; i++) {  /* Note range, the 160 is achieved through 
                                   decoding bit by bit through the char */
      index = j*160 + 16*i;

      unblocked[ index   ]  = (tag->chars[ index / 8 + WD1 ] & m1) != 0 ;
      unblocked[ index+1 ]  = (tag->chars[ index / 8 + WD1 ] & m2) != 0 ;
      unblocked[ index+2 ]  = (tag->chars[ index / 8 + WD1 ] & m3) != 0 ;
      unblocked[ index+3 ]  = (tag->chars[ index / 8 + WD1 ] & m4) != 0 ;
      unblocked[ index+4 ]  = (tag->chars[ index / 8 + WD1 ] & m5) != 0 ;
      unblocked[ index+5 ]  = (tag->chars[ index / 8 + WD1 ] & m6) != 0 ;
      unblocked[ index+6 ]  = (tag->chars[ index / 8 + WD1 ] & m7) != 0 ;
      unblocked[ index+7 ]  = (tag->chars[ index / 8 + WD1 ] & m8) != 0 ;
      unblocked[ index+8 ]  = (tag->chars[ index / 8 + WD2 ] & m1) != 0 ;
      unblocked[ index+9 ]  = (tag->chars[ index / 8 + WD2 ] & m2) != 0 ;
      unblocked[ index+10 ] = (tag->chars[ index / 8 + WD2 ] & m3) != 0 ;
      unblocked[ index+11 ] = (tag->chars[ index / 8 + WD2 ] & m4) != 0 ;
      unblocked[ index+12 ] = (tag->chars[ index / 8 + WD2 ] & m5) != 0 ;
      unblocked[ index+13 ] = (tag->chars[ index / 8 + WD2 ] & m6) != 0 ;
      unblocked[ index+14 ] = (tag->chars[ index / 8 + WD2 ] & m7) != 0 ;
      unblocked[ index+15 ] = (tag->chars[ index / 8 + WD2 ] & m8) != 0 ;
    }
  }

  return 0;
}
int blocksea( short int *unblocked) 
{
  int j;
  for (j = 0; j < 160*160; j++) {
    unblocked[j] = NESSEA ;
  } 

  return 0;
}
int blockland( short int *unblocked) 
{
  int j;
  for (j = 0; j < 160*160; j++) {
    unblocked[j] = NESLAND ;
  } 

  return 0;
}
