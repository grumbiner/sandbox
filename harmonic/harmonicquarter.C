#include "ncepgrids.h"

#define PERIOD 365.256
#define NPER   60

int main (int argc, char *argv[]) {
  FILE *fin;
  global_quarter<float> sst;
  global_quarter<int> count;

  mvector<global_quarter<double> > sum_sin(NPER), sum_cos(NPER);
  
  double fsin[NPER], fcos[NPER];

  ijpt loc;
  double offset = 273.15;
  float tcrit = offset + 2.00;
  int i, j;

// -------------------------------------------
  //printf("Entered the main program\n"); fflush(stdout);

  fin = fopen(argv[1],"r");
  sst.binin(fin);
  fclose(fin);

  if (argc == 3) {
    fin = fopen(argv[2],"r+");
    count.binin(fin);
    for (i = 0; i < NPER; i++) {
      sum_sin[i].binin(fin);
      sum_cos[i].binin(fin);
    }
  }
  else {
    fin = fopen("count","w");
    count.set(0);
    for (i = 0; i < NPER; i++) {
      sum_sin[i].set( 0.0);
      sum_cos[i].set( 0.0);
    }
  }

// Have now read in or initialized all grids
// SST processing:
  count += 1;
  int step = count.gridmax();
  // For fourier analysis:
  for (i = 1; i <= NPER; i++) {
    fsin[i-1] = sin(2.*M_PI*i*step / PERIOD) * 2./PERIOD;
    fcos[i-1] = cos(2.*M_PI*i*step / PERIOD) * 2./PERIOD;
  }

  //printf("about to try to work with the harmonic terms\n"); fflush(stdout);
// Now start summing the harmonic terms:
  for (loc.j = 0; loc.j < count.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < count.xpoints(); loc.i++) {
    
    for (i = 0; i < NPER; i++) {
      sum_sin[i][loc] += (sst[loc] - offset)*fsin[i];
      sum_cos[i][loc] += (sst[loc] - offset)*fcos[i];
    }

  }
  }

  rewind(fin);
  count.binout(fin);
  for (i = 0; i < NPER; i++) {
    sum_sin[i].binout(fin);
    sum_cos[i].binout(fin);
  }
  fclose(fin);

  return 0;
}
