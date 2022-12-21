#include "ncepgrids.h"

// Read in arg list of analyses and construct
// a grand global evaluation of shannon information in the series.
// Take advantage of concentrations being in [0-100] 

int main(int argc, char *argv[]) {
  global_ice<float> conc;
  global_ice<float> info;
  global_ice<mvector<short int> > hist;
  
  FILE *fin;
  int i, j = 0;
  float p;
  ijpt loc;

  for (loc.j = 0; loc.j < conc.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < conc.xpoints(); loc.i++) {
     hist[loc].resize(101);
     hist[loc] = 0;
  }
  }
  
  for (j = 1; j < argc; j++) {
    fin = fopen(argv[j],"r");
    conc.binin(fin);
    fclose(fin);
    if (conc.gridmax() > 3.0) {
      conc /= 100.;
    }

    for (loc.j = 0; loc.j < conc.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < conc.xpoints(); loc.i++) {
      if (conc[loc] <= 1.0) {
        hist[loc][rint(100.*conc[loc]) ] += 1;
      }
    }
    }
  }

// compute shannon information -sum(p*log(p)) at each grid point
  for (loc.j = 0; loc.j < conc.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < conc.xpoints(); loc.i++) {
    info[loc] = 0.0;

    for (i = 0; i < 101; i++) {
      p          = (float) hist[loc][i] / (float) (argc-1); // probabilities
      if (p > 0) {
        info[loc] += -p*log(p);
      }
    }

  }
  }
  info /= log(2.);

  fin = fopen("shannon.out","w");
  info.binout(fin);
  fclose(fin);

  printf("info %f %f %f %f  %f\n",info.gridmin(), info.gridmax(), info.average(), info.rms(), log((float)(argc-1)) / log(2.0) );

  return 0;
}
