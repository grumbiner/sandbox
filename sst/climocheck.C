#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  global_12th<float> t, var, grad;
  int i;

    fin = fopen(argv[1],"r");
    t.ftnin(fin);
    fclose(fin);
    printf("t max min etc. %f %f %f %f\n",t.gridmax(), t.gridmin(), t.average(), t.rms() );

    fin = fopen(argv[2],"r");
    var.ftnin(fin);
    fclose(fin);
    printf("var max min etc. %f %f %f %f\n",var.gridmax(), var.gridmin(), var.average(), var.rms() );

    fin = fopen(argv[3],"r");
    grad.ftnin(fin);
    fclose(fin);
    grad *= 1e5; // convert to K/100 km
    printf("grad max min etc. %f %f %f %f\n",grad.gridmax(), grad.gridmin(), grad.average(), grad.rms() );

// Scan for points that are exceptional:
  ijpt loc;
  latpt ll;
  float tmax = 35.0, tmin = -1.9; // tmin = tfreeze(34.6)
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

  ll.lat = -85.0;
  ll.lon = 0.;
  loc = t.locate(ll);
  printf("antarctic plateau temp: %e\n",t[loc]);


  
  return 0;
}
