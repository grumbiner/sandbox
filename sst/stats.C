#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  global_quarter<float> var2, var3, var4;
  global_quarter<float> sd, skew, kurtosis;
  FILE *fin2, *fin3, *fin4, *fout;
  ijpt loc;

  fin2 = fopen(argv[1], "r");
  fin3 = fopen(argv[2], "r");
  fin4 = fopen(argv[3], "r");
  fout = fopen(argv[4], "w");

  var2.binin(fin2);
  var3.binin(fin3);
  var4.binin(fin4);

  sd.set((float) 0.0);
  skew.set((float) 0.0);
  kurtosis.set((float) 0.0);

  for (loc.j = 0; loc.j < sd.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < sd.xpoints(); loc.i++) {
    if (var2[loc] < 0) {
      printf("negative variance?! %d %d  %f\n",loc.i, loc.j, var2[loc]);
      var2[loc] = 0.0;
    }
    sd[loc]  = sqrt(var2[loc]);
    skew[loc] = var3[loc] / pow((double) var2[loc], (double) 1.5);
    if (var2[loc] > 1.e-2) {
      kurtosis[loc] = var4[loc] / (var2[loc] * var2[loc]) - 3.0;
    }
  }
  }
  printf("kurtosis max, min, avg %f %f %f %f\n",kurtosis.gridmax(), kurtosis.gridmin(), kurtosis.average(), kurtosis.rms() );


  sd.binout(fout);
  skew.binout(fout);
  kurtosis.binout(fout);

  return 0;
}
