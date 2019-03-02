#include <stdio.h>

/* Check of bit ordering */
/* Robert Grumbine 7 Feb 1997 */

int main(void)
{
  short int unblocked[8];
  int i, j;
  unsigned char x;

  unsigned char m1=1;
  unsigned char m2=2;
  unsigned char m3=4;
  unsigned char m4=8;
  unsigned char m5=16;
  unsigned char m6=32;
  unsigned char m7=64;
  unsigned char m8=128;
  int index ;

  x = 127;
  index = 0;

      unblocked[ index   ] = ( x & m1) != 0 ;
      unblocked[ index+1 ] = ( x & m2) != 0 ;
      unblocked[ index+2 ] = ( x & m3) != 0 ;
      unblocked[ index+3 ] = ( x & m4) != 0 ;
      unblocked[ index+4 ] = ( x & m5) != 0 ;
      unblocked[ index+5 ] = ( x & m6) != 0 ;
      unblocked[ index+6 ] = ( x & m7) != 0 ;
      unblocked[ index+7 ] = ( x & m8) != 0 ;

  for ( i = 0; i < 8; i++) {
    printf("%d %d\n",i, unblocked[i]);
  } 

  return 0;
}
