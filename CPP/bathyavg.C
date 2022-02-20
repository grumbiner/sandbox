#include <stdlib.h>
#include "ncepgrids.h"
#include "resops.h"

// Experiment with bathymetric averaging
// Robert Grumbine 21 August 2003

///////////// Utilities
// Compute a bathymetry for the target grid
void bathyfigure(char *fname, TARGET<float> &bathy,
                 TARGET<unsigned char> &mask, float power); 

///////////////////////////////////////////////////////////////////

int main(int argc, char *argv[]) {
#define NPOWERS 4
  TARGET<unsigned char> mask[NPOWERS];
  TARGET<float> bathy[NPOWERS];
  mvector<float> powers(NPOWERS);

  latpt ll;
  ijpt loc;
  unsigned char coast = 0;
  unsigned char boundary = 1;
  unsigned char land = 5;
  unsigned char water = 17;
  unsigned char ocean = 15;
  unsigned char undef = 3;
  palette<unsigned char> gg(19, 65);
  char fname[900];
  int i;
  float toler = 0.1;

////////////////////////////////////////
  powers[0] = -0.5;
  powers[1] =  0.5;
  powers[2] =  1.0;
  powers[3] = -1.0;
  for (i = 0; i < NPOWERS; i++) {
    bathyfigure(argv[1], bathy[i], mask[i], powers[i]);
    bathy[i] *= 128. / 10000.; // scale to 0-128 w. 10000 being 128
    sprintf(fname,"bathymetry%d.xpm",i);
    bathy[i].xpm(fname,7,gg);
    bathy[i] /= 128. / 10000.;
  }

// Now scale the others against reference and look for outlier deviations
    for (loc.j = 0; loc.j < bathy[0].ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < bathy[0].xpoints(); loc.i++) {
       if (bathy[0][loc] != 0) {
         if (fabs((bathy[1][loc]/bathy[0][loc]) - 1.) > toler ||
             fabs((bathy[2][loc]/bathy[0][loc]) - 1.) > toler ||
             fabs((bathy[3][loc]/bathy[0][loc]) - 1.) > toler )  {
           ll = bathy[i].locate(loc);
           printf("%3d %3d  %6.2f %7.2f  %6.1f %6.1f %6.1f %6.1f  %6.3f %6.3f %8.3f\n",
                loc.i, loc.j, 
                ll.lat, ll.lon,
                bathy[0][loc], 
                bathy[1][loc], 
                bathy[2][loc], 
                bathy[3][loc],
                bathy[1][loc]/bathy[0][loc] -1. , 
                bathy[2][loc]/bathy[0][loc] - 1., 
                bathy[3][loc]/bathy[0][loc] - 1. );
         }
         bathy[1][loc] = (bathy[1][loc]/bathy[0][loc]) - 1. ;
         bathy[2][loc] = (bathy[2][loc]/bathy[0][loc]) - 1. ;
         bathy[3][loc] = (bathy[3][loc]/bathy[0][loc]) - 1. ;
       }
       else {
         bathy[1][loc] = 0.; // identical over land
         bathy[2][loc] = 0.; // identical over land
         bathy[3][loc] = 0.; // identical over land
       }
    }
    }

 FILE * fout = fopen("temp", "w");
 bathy[3].binout(fout);
  
  for (i = 1; i < NPOWERS; i++) {
    printf("relative %d max, min, average %f %f %f\n",i,bathy[i].gridmax(),
             bathy[i].gridmin(), bathy[i].average(0.) );
    bathy[i].scale();
    sprintf(fname,"ratio%d.xpm",i);
    bathy[i].xpm(fname,7,gg);
  }

  return 0;
}

//////////////////////////
// Given a bathymetry, construct a target grid bathymetry
void bathyfigure(char *fname, TARGET<float> &bathy,
                 TARGET<unsigned char> &mask, float power) {
  TARGET<float> count;
  TARGET<float> sum;
// Etopo2 input assumed
  llgrid<short int> topo(10800, 5400, -1./30., 1./30., 90.0, -180.0);
  unsigned char coast = 0;
  unsigned char boundary = 1;
  unsigned char land = 5;
  unsigned char water = 17;
  unsigned char ocean = 15;

  FILE *fin;
  ijpt loc, iloc;
  fijpt ij1;
  latpt llat;
  int pass = 0;

  fin = fopen(fname, "r");
  topo.binin(fin);
  fclose(fin);

  sum.set(0);
  count.set(0);
  bathy.set( (float) 0.);

  printf("topography max, min, average %d %d %d\n",topo.gridmax(), 
          topo.gridmin(), topo.average() ); fflush(stdout);
  if (topo.gridmin() == topo.gridmax() ) {
    printf("topo max and min are identical, quitting!\n");
    exit(2);
  }

  mask.set(ocean);

  for (loc.j = 0; loc.j < topo.ypoints(); loc.j++) {
  //printf("j = %d\n",loc.j);
  for (loc.i = 0; loc.i < topo.xpoints(); loc.i++) {
    llat = topo.locate(loc);
    ij1  = bathy.locate(llat);
    iloc.i = (int) (0.5 + ij1.i);
    iloc.j = (int) (0.5 + ij1.j);
    if (bathy.in(ij1) && mask[iloc] != land ) {
      if (topo[loc] >= 0) {
        mask[iloc] = land;
        count[iloc] = 0;
      }
      else { 
        sum[iloc] += pow(fabs(topo[loc]), power) ;
        count[iloc] += 1;
      }
    }
  }
  }

  printf("done summing topographies, power = %f\n", power); fflush(stdout);
  for (loc.j = 0; loc.j < sum.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < sum.xpoints(); loc.i++) {
    if (count[loc] == 0 && mask[loc] != land ) {
      printf("Failed to find any valid topography information at %d %d!\n",
          loc.i, loc.j); fflush(stdout);
      bathy[loc] = 0.;
      mask[loc] = land;
    }
    else if (count[loc] == 0) {
      bathy[loc] = 0.;
      mask[loc] = land;
    }
    else {
      bathy[loc] = pow(sum[loc] / count[loc], 1./power);
    }
  }
  }

  return;     
}
