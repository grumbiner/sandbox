#include "ncepgrids.h"

void calc(global_12th<float> &x, float &area, float &extent) ;

int main(void) {
  FILE *fin;
  global_12th<float> ice;
  global_12th<float> nh, sh;
  float area, extent;

  fin = fopen("fred","r");
  ice.binin(fin);
  fclose(fin);

  ijpt loc; 
  latpt ll;
  for (loc.j = 0; loc.j < ice.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ice.xpoints() ;loc.i++) {
    ll = ice.locate(loc);
    if (ll.lat > 5.0) {
      nh[loc] = 1.;
      sh[loc] = 0.;
    }
    else {
      nh[loc] = 0.;
      sh[loc] = 1.;
    }
  }
  }
  nh *= ice;
  sh *= ice;

  calc(ice, area, extent);
  printf("global %f %f\n",area / 1.e6, extent / 1.e6);
  calc(nh, area, extent);
  printf("nhem   %f %f\n",area / 1.e6, extent / 1.e6);
  calc(sh, area, extent);
  printf("shem   %f %f\n",area / 1.e6, extent / 1.e6);


  return 0;
}
void calc(global_12th<float> &x, float &area, float &extent) {
  double sum1 = 0.0, sum2 = 0.0;
  ijpt loc;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (x[loc] > 0.) {
      sum1 += x[loc]*x.cellarea(loc);
      sum2 +=        x.cellarea(loc);
    }
  }
  }
  area = (float) sum1;
  extent = (float) sum2;
  return;
}
