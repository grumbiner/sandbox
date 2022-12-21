#include <stdio.h>
#include <stdlib.h>
#ifdef W3LIB
  #include <time.h>
#endif

#include "ncepgrids.h"

// General purpise ice field engribber 
//   Read in file, write back out in grib
//   Argument list: input_file output_file YYYY MM DD

// Robert Grumbine 18 April 2001 

//////////////////////////////////
// Something to incorporate in to a library
template <class T, class U>
void gridset(grid2_base<T> &x, grid2_base<U> &y);
//////////////////////////////////


int main(int argc, char *argv[])
{
  FILE *fin;

  GRIDTYPE<DATTYPE> nland;
  GRIDTYPE<float> nlandf;

  #ifdef W3LIB
    char *grib;
    int lgrib = 0;
    tm*  date;
    FILE *fgrib;
    int parmno = 255, mxbit = 8, depth = 0, lead = 0;
  #endif

  fin = fopen(argv[1], "r");
  nland.binin(fin);
  fclose(fin);
  gridset(nlandf, nland);

// If the W3LIB is defined, we should write out a grib version 
  #ifdef W3LIB

    fgrib = fopen(argv[2], "w");
    if (fgrib == (FILE *) NULL) {
      printf("Failed to open the grib output file\n");
      return 1;
    }
    printf("Gribbing file for %d %d %d\n",atoi(argv[3]), atoi(argv[4]), atoi(argv[5]) );
    date->tm_year = atoi(argv[3]);
    date->tm_mon  = atoi(argv[4]);
    date->tm_mday = atoi(argv[5]);
    date->tm_hour = 0;
    date->tm_min  = 0; 
    nlandf.pds.set_precision(0.01);
    nlandf.pds.set_time(date->tm_year, date->tm_mon, date->tm_mday,
                       date->tm_hour, date->tm_min);

    if (nlandf.average() > 2.0) nlandf /= 100.;
    grib = new char [ (nlandf.xpoints() * nlandf.ypoints())/8 + 200 ];
    nlandf.gribit(parmno, depth, lead, grib, lgrib, mxbit);
    delete []grib;

  #else
    printf("You need to compile with -DW3LIB and attach the w3lib\n");
  #endif

  return 0;

}

//Set grid x = grid y, regardless of types
template <class T, class U>
void gridset(grid2_base<T> &x, grid2_base<U> &y) {
  int i, npts = x.xpoints()*x.ypoints();

  if (x.xpoints() != y.xpoints() || x.ypoints() != y.ypoints() ) {
    x.set((T) 0);
  }
  else {
    for (i = 0; i < npts; i++) {
       x[i] = y[i];
    }
  }
  return;
} 
