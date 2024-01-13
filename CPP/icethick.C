#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  gaussian<float> x, y(254);
  global_ice<float> thick, extent;
  float landval, nonval, ref;
  ijpt loc;

  fin = fopen("icethick","r");
  if (fin == (FILE*) NULL) {
     printf("failed to open input file\n");
     return 1;
  }
  x.binin(fin);
  fclose(fin);
 
  ref = atof(argv[1]);

  //printf("x min, average %f %f\n",x.gridmin(), x.average() );
  landval = x.gridmax(); nonval = landval;
  thick.fromall(x, landval, nonval);

  for (loc.j = 0; loc.j < thick.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < thick.xpoints() ; loc.i++) {
    if (thick[loc] > 2000) thick[loc] = 0;

    if (thick[loc] >= ref ) {
      extent[loc] = 1.0;
    }
    else {
      extent[loc] = 0.0;
      thick[loc] = 0.0;
    }
  }
  }

  float totthick = thick.integrate();
  float totextent = extent.integrate();
  printf("%f  %f %f  %f\n",ref, totthick/1.e12, totextent/1.e12, totthick / totextent);

  return 0;
}
