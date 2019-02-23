#include "ncepgrids.h"
#define PERIOD 365.256
#define NPER   60


int main(int argc, char *argv[]) {
// Data to read in:
  global_quarter<int> count;
// Input mask (land, ocean, inland water):
  global_quarter<unsigned char> orimask;

// Harmonic analyses:
  mvector<global_quarter<double> > ampl(NPER), phase(NPER);
  mvector<global_quarter<double> > sum_sin(NPER), sum_cos(NPER);


// Local and flag values
  FILE *fin, *fout;
  ijpt loc;
  latpt ll;
  float offset = 273.15;
  int i, n;

// Get input data
  fin = fopen(argv[1],"r");
    count.binin(fin);
    for (i = 0; i < NPER; i++) {
      sum_sin[i].binin(fin);
      sum_cos[i].binin(fin);
    }
  fclose(fin);

  fin = fopen(argv[2], "r");
  if ( fin == (FILE *) NULL) {
    printf ("Failed to open land mask file %s\n",argv[2]);
    return 1;
  }
  orimask.binin(fin);
  fclose(fin);

  printf("Have read the input data files\n"); fflush(stdout);
  fout = fopen(argv[3],"w");

// Now being big loop to scan over the previously processed
//   data and compute final statistics, do qc checks, and
//   start making a new mask file:
  for (loc.j = 0; loc.j < count.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < count.xpoints(); loc.i++) {
    //printf("working on point %d %d\n",loc.i, loc.j); fflush(stdout);
    ll = count.locate(loc);

// Compute fourier amplitude and phase:
    for (i = 0; i < NPER; i++) {
       ampl[i][loc] = sqrt(sum_sin[i][loc]*sum_sin[i][loc] + 
                      sum_cos[i][loc]*sum_cos[i][loc]    );
       phase[i][loc] = atan2(sum_sin[i][loc], sum_cos[i][loc] );
    }

  }
  }

////////////////////////////////////////////////////////////////////////
////  Done with straightforward pass through the processed file 
////////////////////////////////////////////////////////////////////////

// Finally, make output files 
  for (i = 0; i < NPER; i++) {
    ampl[i].binout(fout);
    printf("amp %2d max min avg %f %f %f\n",i, ampl[i].gridmax(), ampl[i].gridmin(), ampl[i].average());
    phase[i].binout(fout);
  }  

  return 0;
}
