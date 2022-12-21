#include <stdio.h>

#include "vector.h"
#include "grid_math.h"
#include "ncepgrids.h"

#define DAT_LENGTH 363
#define PARMS 15
#define SCALE 10

// Frame of a program to compute correlation matrix.  Assumes that
//  the eof computation is done elsewhere.
// Robert Grumbine 17 July 2000

float dot(vector<float> &x, vector<float> &y);

int make_cors(vector<vector<float> > &dat, grid2<float> cors, int ndat, int npar) ;

int main(void) {
  global_ice<float> icegrid;
  grid2<vector< vector<float> > > dat(720/SCALE, 360/SCALE); 
  grid2<float> cors(PARMS,PARMS);
  int i, j, k;
  ijpt loc, locs[PARMS];
  latpt lats;
  int spacing = 1;

  FILE *fin;

// Initialize the data grid
//  for (i = 0; i < PARMS; i++) {
//    dat[i].resize(DAT_LENGTH);
//  }
  for (loc.j = 0; loc.j < dat.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < dat.xpoints(); loc.i++) {
    dat[loc].resize(PARMS);
    for (i = 0; i < PARMS; i++) {
      dat[loc][i].resize(DAT_LENGTH);
    }
  }
  }

// Read in data:
  fin = fopen("iceall","r");
  for (j = 0; j < DAT_LENGTH; j++) {
    icegrid.binin(fin);
    for (loc.j = 0; loc.j < dat.ypoints(); loc.j += 1) {
    for (loc.i = 0; loc.i < dat.xpoints(); loc.i += 1) {
      for (i = 0; i < PARMS; i++) {
        locs[i].j = loc.j*SCALE ;
        locs[i].i = loc.i*SCALE + i*spacing;
        dat[loc] [i][j] = icegrid[ locs[i] ];
      }
    }
    }
  }

// Conduct the computations
  for (loc.j = 0; loc.j < dat.ypoints(); loc.j += 1) {
  for (loc.i = 0; loc.i < dat.xpoints(); loc.i += 1) {
    printf("%3d %3d \n",loc.j, loc.i);
    make_cors(dat[loc], cors, DAT_LENGTH, PARMS);
  }
  }
      
  return 0;
}

int make_cors(vector<vector<float> > &dat, grid2<float> cors, 
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

float dot(vector<float> &x, vector<float> &y) {
  int i;
  float sum=0.;
  
  for (i = 0; i < x.xpoints(); i++) {
    sum += x[i]*y[i];
  }
  return sum;
}
