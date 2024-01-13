#include <math.h>

#include "ssmiclass.h"

int main(int argc, char *argv[]) {
  GRIDTYPE<ssmipt> north;
  GRIDTYPE<bool> ok;
  FILE *fin;
  ijpt x;
  int h, v, conc;
  float cross;

  ok.set(true);
  fin = fopen(argv[1],"r");
  north.binin(fin);
  for (x.j = 0; x.j < north.ypoints() ; x.j++) {
  for (x.i = 0; x.i < north.xpoints() ; x.i++) {
     ok[x] = true;
     if (north[x].qc() != 0) {
//       printf("%d failed at %3d %3d ", north[x].qc(), x.i, x.j);
//       north[x].show();
       ok[x] = false;
     }
     conc = (int) north[x].ssmi.conc_bar;
     if (conc != (int) north[x].ssmi.bar_conc &&
         (conc != WEATHER && (int) north[x].ssmi.bar_conc != WEATHER   ) &&
         (conc > 0 && (int) north[x].ssmi.bar_conc > 0   ) 
        ) {
//       printf("delconc %3d %3d  %3d %3d  %4d\n",
//              x.i, x.j, north[x].ssmi.conc_bar, 
//              north[x].ssmi.bar_conc,          
//              north[x].ssmi.conc_bar - north[x].ssmi.bar_conc);
       ok[x] = false;
     }
     if (conc == 157 || conc == 195 || conc == 224 || conc == 177) {
       ok[x] = false;
     }
  }
  }

// Now that we have a set of points which look passable (over 90% of
//   grid, we're not being very choosy) see what can be seen in values:
  for (x.j = 0; x.j < north.ypoints() ; x.j++) {
  for (x.i = 0; x.i < north.xpoints() ; x.i++) {
    if (ok[x]) {
      h = (int) north[x].ssmi.t37h;
      v = (int) north[x].ssmi.t37v;
      conc = (int) north[x].ssmi.conc_bar;
      cross = (h+v)*(h-v);
      printf("%3d %3d ",x.i, x.j);
      printf("%5d %5d  %6d %6d %6.4f",
                                 h,  // 1
                                 v,  // 2
                                 h+v,  // 3
                                 h-v,  // 4
                                 (float)h/(float)v );  // 5
      printf("  %f %d %f %f %f",
                                 cross*cross, // 6  
                                 (h-v)*(h-v), // 7
                                 cross, // 8
                                 (float) h*h+v*v, // 9
                                 cross/( (float) h*h+v*v) );  // 10
      printf(" %f %7.4f %f",
             ( (float)h/(float)v)*((float)h/(float)v), // 11
             (float) (h-v)/(float)(h+v), // 12
             pow((float) (h-v)/(float)(h+v), 2) );  // 13
      printf(" %f %f %f %f", sqrt( (float)h/(float)v ), // 14
                             sqrt(fabs((float)h-v)), // 15
                             sqrt(fabs( (float) (h-v)/(float)(h+v)) ) , // 16
                             sqrt(fabs(cross)) );  // 17
      printf(" %3d\n", conc );
    }
  }
  }

  return 0;
} 
