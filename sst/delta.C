#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  global_quarter<float> ref, orig, delta;
  FILE *fin, *fout;
  ijpt loc, tloc;
  global_quarter<short int> rey_day;
  float rey_flag = -999;
  int x[4];
 
// Climatology to be working from -- in my convention and in degrees C
  fin = fopen(argv[1],"r");
  ref.binin(fin);
  fclose(fin);
  //printf("ref max min %f %f\n",ref.gridmax(), ref.gridmin());

  fin = fopen(argv[2], "r");
  fread(&x, sizeof(int), 4, fin);
  rey_day.binin(fin);
  fclose(fin);
  // flip from reynolds convention to mine:
  for (loc.j = 0; loc.j < rey_day.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < rey_day.xpoints(); loc.i++) {
    tloc.j = rey_day.ypoints() - loc.j - 1;
    tloc.i = loc.i;
    orig[tloc] = rey_day[loc];
    if (orig[tloc] == rey_flag) {
      orig[tloc] = 0.0;
    }

    if (ref[tloc] == rey_flag) {
      ref[tloc] = 0.0;
    }
  }
  }
  orig /= 100.;
  ref /= 100.;
  printf("ref max min %f %f\n",ref.gridmax(), ref.gridmin());
  printf("orig max min %f %f\n",orig.gridmax(), orig.gridmin());

  fout = fopen(argv[3], "w");
  delta = ref;
  delta -= orig;
  delta.binout(fout);
  fclose(fout);

  return 0;

  latpt ll;
  float toler;
  toler = atof(argv[4]);
  if (delta.gridmax() > toler || delta.gridmin() < -toler) {
    for (loc.j = 0; loc.j < ref.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < ref.xpoints(); loc.i++) {
      if (fabs(delta[loc]) > toler) {
        ll = ref.locate(loc);
        printf("%4d %4d  %6.2f %7.2f  %7.2f %7.2f %7.2f\n",loc.i, loc.j, ll.lat, ll.lon, 
           ref[loc], orig[loc], delta[loc]); 
      }
    }
    }
  }

  return 0;
}
