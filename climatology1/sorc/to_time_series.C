#include "ncepgrids.h"

#define NPTS  55233
#define NSTEP 11688

int main(int argc, char *argv[]) {
  mvector<mvector<float> > x(NPTS);
  int i, j;
  global_ice<float> conc;
  mvector<ijpt> locs(NPTS);
  FILE *fin;
  char fname[90];
  FILE *fout;

// Size the time series:
  for (i = 0; i < x.xpoints() ; i++) {
    x[i].resize(NSTEP);
    //printf("%d\n",i); fflush(stdout);
  }


// get the locations:
  fin = fopen("locations","r");
  locs.binin(fin);
  fclose(fin);

// Read in day by day and partition the data:
  for (i = 1; i < NSTEP; i++) {
    sprintf(fname, "conc.%d",i);
    if ( (i % 30) == 0) {
       printf("opening %s\n",fname); fflush(stdout);
    }
    fin = fopen(fname, "r");
    conc.binin(fin);
    fclose(fin);
    for (j = 0; j < NPTS; j++) {
      x[j][i-1] = conc[locs[j] ];
    }
  } 

// Now that all data are in hand, write out as time series
//   labelled by the i,j of the point
  for (i = 0; i < NPTS; i++) {
    sprintf(fname,"conc.%03d.%03d",locs[i].i, locs[i].j);
    fout = fopen(fname, "w");
    x[i].binout(fout);
    fclose(fout);
  }

  return 0;
}
