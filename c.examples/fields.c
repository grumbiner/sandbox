#include "stdio.h"

void main()
{
  struct { unsigned int a : 7 ;
           unsigned int b : 7 ;
           unsigned int c : 7 ;
           unsigned int d : 7 ;
           unsigned int e : 7 ;
           unsigned int f : 7 ;
           unsigned int g : 7 ;
           unsigned int h : 7 ;
           unsigned int i : 7 ;
           unsigned int j : 7 ;
             } bf ;

  bf.a = 200; /* Wraps around to 72 */
  bf.b = 104;
  bf.c = 108;
  bf.d = 112;
  bf.e = 116;
  bf.f = 120;
  bf.g = 124;
  bf.h = 127;
  bf.i = 121;
  bf.j = 121;

  printf (" %d \n", bf.a); 
  printf (" %d \n", bf.b); 
  printf (" %d \n", bf.c); 
  printf (" %d \n", bf.d); 
  printf (" %d \n", bf.e); 
  printf (" %d \n", bf.f); 
  printf (" %d \n", bf.g); 
  printf (" %d \n", bf.h);
  printf (" %d \n", bf.i);
  printf (" %d \n", bf.j);

  printf ("bf is  %d bytes \n", sizeof(bf) );

}
