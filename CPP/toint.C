#include "ncepgrids.h"

void rescale(global_quarter<float> &x, global_quarter<short int> &y) ;

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  global_quarter<float> refin, delta;
  global_quarter<short int> refout;

  fin = fopen(argv[1],"r");
  refin.binin(fin);
  fclose(fin);

  fout = fopen(argv[2], "w");
  rescale(refin, refout);
  refout.binout(fout);
  fclose(fout);

  delta = refin;
  for (int i = 0; i < delta.xpoints() * delta.ypoints() ; i++) {
    delta[i] -= (float) refout[i];
  }
  printf("delta max, min, avg, rms %f %f %f %f\n",delta.gridmax(), delta.gridmin(),
          delta.average(), delta.rms() );

  return 0;
}
  
void rescale(global_quarter<float> &x, global_quarter<short int> &y) {
// rescale field back to the centi-degree short int representation
// -- should have this done for the climatology year as well to preserve
//    full linearity
  ijpt loc;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++ ) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++ ) {
    //y[loc] = (short int) (0.5 + 1.*x[loc]);
    y[loc] = rint(x[loc]);
  }
  }
  return;
}
