#include <stdio.h>

#include "mvector.h"
#include "grid_math.h"
#include "ncepgrids.h"


#define DAT_LENGTH 363
#define PARMS 5

// Frame of a program to compute correlation matrix.  Assumes that
//  the eof computation is done elsewhere.
// Robert Grumbine 17 July 2000

float dot(mvector<float> &x, mvector<float> &y);

int make_cors(mvector<mvector<float> > &dat, grid2<float> cors, int ndat, int npar) ;

int main(void) {
  global_ice<float> icegrid;
  mvector< mvector<float> > dat(PARMS+1);
  grid2<float> cors(PARMS+1,PARMS+1);
  int i, j;
  ijpt loc, locs[PARMS+1];
  latpt lats;
  int spacing=2;

  FILE *fin;

  for (i = 0; i < PARMS+1; i++) {
    dat[i].resize(DAT_LENGTH);
  }

// Read in data:
  lats.lat = -75.0;
  lats.lon = -180.0;
  loc = icegrid.locate(lats);
  //printf("loc.i, j = %d %d\n",loc.i, loc.j);
  for (i = 0; i < PARMS+1; i++) {
   locs[i].j = loc.j;
   locs[i].i = loc.i + i*spacing;
  }

  fin = fopen("iceall","r");
  for (j = 0; j < DAT_LENGTH; j++) {
    icegrid.binin(fin);
    for (i = 0; i < PARMS+1; i++) {
      dat[i][j] = icegrid[locs[i] ];
    }
  }
      
    
  make_cors(dat, cors, DAT_LENGTH, PARMS+1);

  return 0;
}

int make_cors(mvector<mvector<float> > &dat, grid2<float> cors, 
                    int ndat, int npar) {
  int i, j;
  ijpt loc, sloc;
  float rescale = sqrt(ndat);

// De-mean and normalize:
  for (i = 0; i < npar; i++) {
    dat[i] -= dat[i].average();
    dat[i].normalize();
    dat[i].rescale(rescale);
  }

// Set up the correlation matrix
  for (j = 0; j < npar; j++) {
    loc.j = j; loc.i = j;
    cors[loc] = 1.0;
  }
  for (loc.j = 0; loc.j < npar; loc.j++) {
  for (loc.i = loc.j+1; loc.i < npar; loc.i++) {
    cors[loc] = dot(dat[loc.i], dat[loc.j]);
    sloc.j = loc.i;
    sloc.i = loc.j;
    cors[sloc] = cors[loc];
  }
  }

// Print out the correlation matrix:
  for (loc.j = 0; loc.j < npar; loc.j++) {
  for (loc.i = 0; loc.i < npar; loc.i++) {
    printf("%f ",cors[loc]);
  }
  printf("\n");
  }

  return 0;
}

float dot(mvector<float> &x, mvector<float> &y) {
  int i;
  float sum=0.;
  
  for (i = 0; i < x.xpoints(); i++) {
    sum += x[i]*y[i];
  }
  return sum;
}

  
