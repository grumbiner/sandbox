#include "ncepgrids.h"
// Robert Grumbine 21 October 2008
// Somewhat generalized histogram computation on ice concentrations

int main(int argc, char *argv[]) {
  GRIDTYPE<DTYPE> conc;
  FILE *fin;
  mvector<int> hist(5000);
  int i;
  ijpt loc;
  float flag = -1.0;
  latpt ll;

  fin = fopen(argv[1],"r");
  conc.binin(fin);
  fclose(fin);
  printf("read the data in\n"); fflush(stdout);

// Now process for flag values, magnitude, and range:
  if (conc.gridmax() > 256) {
    flag = conc.gridmax();
    printf("flagged data set %f\n",flag);
  }
  printf("%f %f %f\n",conc.gridmax(flag), conc.gridmin(flag), conc.average(flag) ); fflush(stdout);
  if (conc.gridmax(flag) < 100.) {
     conc *= 100;
     flag *= 100;
     printf("rescaled flag %f\n",flag);
  } 
  if (conc.gridmin(flag) < 0.) {
     //flag -= conc.gridmin(flag);
     //printf("shift values up by %f\n",conc.gridmin(flag) );
     //conc -= conc.gridmin(flag);
     //printf("added to flag %f\n",flag);
     flag -= -100.0;
     conc -= -100.0; // negatives are velocities, assume that 1 m/s is max possible
  }
  fflush(stdout);


  hist = 0;
  printf("starting the loop\n"); fflush(stdout);
  for (loc.j = 0; loc.j < conc.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < conc.xpoints(); loc.i++) {
    //if (conc[loc] == flag) continue;
    if (conc[loc] >= hist.xpoints() ) continue;
    //printf("%d %d %f\n",loc.i, loc.j, conc[loc] ); fflush(stdout);
    ll = conc.locate(loc);
    if (ll.lat < 0) continue;

    if ( (int) (0.5 + conc[loc]) > hist.xpoints() ) {
      printf("overran the histogram limits, %f\n", conc[loc] );
      fflush(stdout);
    }
    hist[ (int) (0.5 + conc[loc]) ] += 1;
  }
  }

// Print out histogram, nonzero values only:
  for (i = 0; i < hist.xpoints(); i++) {
    if (hist[i] != 0) printf("%5d %d\n",i,hist[i]);
  }

  return 0;
} 
