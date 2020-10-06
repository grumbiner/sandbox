#include <stdio.h>
#include <stdlib.h>

#include "icessmi.h"
#include "ncepgrids.h"

int main(int argc, char *argv[])
{
  palette<unsigned char> gg(19, 65);

  northgrid<unsigned char> nland;
  northgrid<float> nmap, nf, count, nlandf;
  southgrid<unsigned char> sland;
  southgrid<float> smap, sf, scount, slandf;
  global_ice<unsigned char> cout2, gmap, refmap;
  global_ice<float> outmap, altmap;
  global_sst<float> sst, errs; 

  ijpt ijloc, destloc, ijloc2;
  fijpt tloc;
  latpt iceloc, outloc;
  unsigned char tmp;
  float filt_temp, flag;
  int range = 0; //min distance to land for a valid concentration
  int isolates, filled;

  #ifdef VERBOSE
    printf("Entered the filtering program\n"); fflush(stdout);
  #endif

  return 0;
}
