#include "ncepgrids.h"

// Read in arg list of analyses and construct
// a grand global histogram by concentration.

int main(int argc, char *argv[]) {
  global_ice<float> conc;
  mvector<double> hist(256);
  
  FILE *fin;
  int i, j = 0;
  ijpt loc;

  hist = 0;

  for (j = 1; j < argc; j++) {
    fin = fopen(argv[j],"r");
    conc.binin(fin);
    fclose(fin);
    if (conc.gridmax() > 3.0) {
      conc /= 100.;
    }
    //Debug printf("%3d %f \n",j, conc.gridmax() ); 

    for (loc.j = 0; loc.j < conc.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < conc.xpoints(); loc.i++) {
      //hist[rint(100.*conc[loc]) ] += conc[loc]*conc.cellarea(loc) ;
      hist[rint(100.*conc[loc]) ] += conc.cellarea(loc) ;
    }
    }

  }
  hist /= (argc-1); // average daily histogram

  double sum=0;
  for (i = 1; i < 102; i++) {
    if (hist[i] != 0) {
      sum += hist[i];
      printf("%3d  %e %e\n",i,hist[i]/1.e12, sum/1.e12);
    }
  }

  return 0;
}
