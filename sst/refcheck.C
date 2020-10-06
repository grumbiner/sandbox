#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  global_quarter<float> t[15];
  int i = 0, mean = 0, trend = 1, annual = 2, semi = 4;
  ijpt loc;
  latpt ll;

// antarctic plateau data:
  ll.lat = -85.0;
  ll.lon = 0.;
  loc = t[0].locate(ll);

  fin = fopen(argv[1],"r");
  while (!feof(fin)) {
    t[i].binin(fin);
    if (feof(fin)) break;
    printf("%d max min etc. %e %e %e %e\n",i, t[i].gridmax(), t[i].gridmin(), t[i].average(), t[i].rms() );
    printf("%d plateau value: %e\n",i, t[i][loc]);

    i++;
  }
  fclose(fin);

// check for oddly small, but nonzero, values:
  for (loc.j = 0; loc.j < t[mean].ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < t[mean].xpoints(); loc.i++) {
    if (fabs(t[mean][loc]) > 0 && fabs(t[mean][loc]) < 1.e-10) {
      ll = t[mean].locate(loc);
      printf("%4d %4d  %7.3f %7.3f  mean %e\n",loc.i, loc.j, ll.lon, ll.lat, t[mean][loc]);
    }
    if (fabs(t[trend][loc]) > 0 && fabs(t[trend][loc]) < 1.e-15) {
      ll = t[trend].locate(loc);
      printf("%4d %4d  %7.3f %7.3f  trend %e\n",loc.i, loc.j, ll.lon, ll.lat, t[trend][loc]);
    }
  }
  }


// look for points where variance in semiannual cycle is greater than for annual:
  for (loc.j = 0; loc.j < t[mean].ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < t[mean].xpoints(); loc.i++) {
    if (t[semi][loc] > t[annual][loc]) {
      ll = t[semi].locate(loc);
      printf("%4d %4d  %7.3f %7.3f  ann %e semi %e ratio %f\n",loc.i, loc.j, ll.lon, ll.lat, t[annual][loc], t[semi][loc], t[annual][loc]/t[semi][loc]);
    }
  }
  }
 



  return 0;

}
void scan(void) {
// Scan for points that are exceptional:
  float tmax = 35.0, tmin = -1.9; // tmin = tfreeze(34.6)
  ijpt loc;
  latpt ll;
  global_quarter<float> t, var, grad;

  for (loc.j = 0; loc.j < t.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < t.xpoints(); loc.i++) {
    if (t[loc] > tmax || t[loc] < tmin) {
      ll = t.locate(loc);
      printf("%4d %4d  %7.3f %7.3f  t %6.3f\n",loc.i, loc.j, ll.lon, ll.lat, t[loc]);
    }
    if (var[loc] < 0) {
      ll = var.locate(loc);
      printf("%4d %4d  %7.3f %7.3f  var %f\n",loc.i, loc.j, ll.lon, ll.lat, var[loc]);
    }
    if (grad[loc] < 0) {
      ll = grad.locate(loc);
      printf("%4d %4d  %7.3f %7.3f  grad %f\n",loc.i, loc.j, ll.lon, ll.lat, grad[loc]);
    }
     
  }
  }
  
}
