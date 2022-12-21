#include <stdio.h>
#include <stdlib.h>
#ifdef W3LIB
  #include <time.h>
#endif

#include "icessmi.h"
#include "ncepgrids.h"

/* The land.pathology engribbing fails on the SP, apparently because of
   a memory overrun.  This code reads in a field of GRIDTYPE<DATTYPE>
   and writes it back out.  Assumes that the field is land percentage 
   -> parm81 */
// Read in the associated mask file to ensure that pathological points
// get remapped to 100% land.
/* Robert Grumbine 13 April 2001 */


//////////////////////////////////
// Something to incorporate in to a library
template <class T, class U>
void gridset(grid2_base<T> &x, grid2_base<U> &y);
//////////////////////////////////


int main(int argc, char *argv[])
{
  FILE *fin;

  GRIDTYPE<DATTYPE> nland;
  GRIDTYPE<float> nlandf, landpct;

  ijpt loc;

  #ifdef W3LIB
    char *grib;
    int lgrib = 0;
    tm*  date;
    time_t secs;
    FILE *fgrib;
    int parmno = 81, mxbit = 8, depth = 0, lead = 0;
  #endif

  fin = fopen(argv[1], "r");
  landpct.binin(fin);
  fclose(fin);
  fin = fopen(argv[2],"r");
  nland.binin(fin);
  fclose(fin);
  gridset(nlandf, nland);
  if (nlandf.average() < 3.0) {nlandf *= 100.;}
 
// To ensure consistency between land mask and land percentage, if
//  land % is less than 40 but land flag is 'land', turn % to 80.
  for (loc.j = 0; loc.j < landpct.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < landpct.xpoints(); loc.i++) {
    if (nlandf[loc] == LAND && landpct[loc] < 0.40) {
      printf("%d %d  %f  %f\n",loc.i, loc.j, nlandf[loc], landpct[loc]);
      landpct[loc] = 0.8;  
    }
  }
  }



// If the W3LIB is defined, we should write out a grib version of
//   the filtered file.  Note that this also requests a fourth argument.
  #ifdef W3LIB
    grib = new char [ (landpct.xpoints() * landpct.ypoints())/8 + 200 ];

    fgrib = fopen(argv[3], "w");
    if (fgrib == (FILE *) NULL) {
      printf("Failed to open the grib output file\n");
      return 1;
    }
    secs = time(&secs);
    date = gmtime(&secs);
    date->tm_year += 1900;  //Need to add this and next because of referencing
    date->tm_mon  += 1;
    landpct.pds.set_precision(0.01);
    landpct.pds.set_time(date->tm_year, date->tm_mon, date->tm_mday,
                       date->tm_hour, date->tm_min);

    if (landpct.average() > 2.0) landpct /= 100.;
    printf("About to call gribit\n"); fflush(stdout);
    landpct.gribit(parmno, depth, lead, grib, lgrib, mxbit);
    printf("back from gribit\n"); fflush(stdout);

    printf(" wrote a %d length grib message\n",
           fwrite(grib, sizeof(char), lgrib, fgrib)  ) ; fflush(stdout);
    printf("lgrib = %d\n",lgrib); fflush(stdout);
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
