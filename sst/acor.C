#include "ncepgrids.h"

int main(void) {
  FILE *fin;
  int i;
  global_quarter<float> acor[35];
  global_quarter<float> tau;
  double sum = 0.0, area = 0.0;
  ijpt loc;
  float efold = 1./2.781828;

  fin = fopen("autorrelations", "r");

  for (i = 0; i < 35; i++) {
    acor[i].binin(fin);
    printf("%2d %f %f %f\n",i,acor[i].gridmax(), acor[i].gridmin(), acor[i].average() );
    sum = 0.0; area = 0.0;
    for (loc.j = 0; loc.j < tau.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < tau.xpoints(); loc.i++) {
       if (acor[i][loc] != 0.0) {
         area += tau.cellarea(loc);
         sum  += acor[i][loc] * tau.cellarea(loc);
       }
    }
    }
    printf("%2d average correl %f\n",i, (float) (sum / area) );

  }
  fclose(fin);

  tau.set((float) -1.0);
  for (loc.j = 0; loc.j < tau.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < tau.xpoints(); loc.i++) {
    for (i = 0; i < 35; i++) {
      if (tau[loc] == -1 && acor[i][loc] < efold) {
        tau[loc] = i;
        break;
      } 
    }
  }
  }

  for (loc.j = 0; loc.j < tau.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < tau.xpoints(); loc.i++) {
    if (tau[loc] > 0. ) {
      printf("%4d %4d  %3.0f\n",loc.i, loc.j, tau[loc]);
    }
  }
  }

  return 0;
}
