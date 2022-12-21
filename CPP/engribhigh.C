#include <stdio.h>
#include <stdlib.h>
#ifdef W3LIB
  #include <time.h>
#endif

#include "icessmi.h"
#include "ncepgrids.h"
#define MAX_ICE 128
#define MIN_CONC 15

/* The land.pathology engribbing fails on the SP, apparently because of
   a memory overrun.  This code reads in a field of GRIDTYPE<DATTYPE>
   and writes it back out.  Assumes that the field is sea ice */
/* Robert Grumbine 13 April 2001 */

/* Filter the global ice concentration grid according to the SST and
    to the land mask */
/* Note that hooks are in place for filtering the hemispheric ice grids,
    though this isn't done currently. */
/* Robert Grumbine 4 June 1997 */
/* Major revision: change to C++, go to wgrib decoding of sst files
   Robert Grumbine 8 September 1999 */

/* Function to fill in the large number of single-point dropouts (bad,
     missing, or weather) that are surrounded by valid ice concentrations */
template <class T>
int gapfill(grid2<T> &x, T lowest_valid, T highest_valid, T flag);
template <class T>
int isolani(metricgrid<T> &x, T flag, T replace); // remove points which 
               //are surrounded by flagged values and replace 
template <class T>
void weather_zero(grid2<T> &x);

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

  ijpt ijloc, destloc, ijloc2, x;
  fijpt tloc;
  latpt iceloc, outloc;
  unsigned char tmp;
  float filt_temp, flag;
  int range = 0; //min distance to land for a valid concentration
  int isolates, filled;

  #ifdef W3LIB
    char *grib;
    int lgrib = 0;
    tm*  date;
    time_t secs;
    FILE *fgrib;
    int parmno = 255, mxbit = 8, depth = 0, lead = 0;
  #endif

  fin = fopen(argv[1], "r");
  nland.binin(fin);
  fclose(fin);
  gridset(nlandf, nland);

// If the W3LIB is defined, we should write out a grib version of
//   the filtered file.  Note that this also requests a fourth argument.
  #ifdef W3LIB
    grib = new char [ (nlandf.xpoints() * nlandf.ypoints())/8 + 200 ];

    fgrib = fopen(argv[2], "w");
    if (fgrib == (FILE *) NULL) {
      printf("Failed to open the grib output file\n");
      return 1;
    }
    secs = time(&secs);
    date = gmtime(&secs);
    date->tm_year += 1900;  //Need to add this and next because of referencing
    date->tm_mon  += 1;
    nlandf.pds.set_precision(0.01);
    nlandf.pds.set_time(date->tm_year, date->tm_mon, date->tm_mday,
                       date->tm_hour, date->tm_min);

    if (nlandf.average() > 2.0) nlandf /= 100.;
    printf("About to call gribit\n"); fflush(stdout);
    nlandf.gribit(parmno, depth, lead, grib, lgrib, mxbit);
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
