#include "ncepgrids.h"
#include "ssmiclass.h"

int main(void) {
  FILE *fin;
  northhigh<ssmipt> x;
  mvector<mvector<int> > y(x.xpoints() * x.ypoints()) ;
  ssmipt tmax, tmin;
  ijpt loc;
  int i, recount = 0, count = 0;
  float radius = 10*100;
  mvector<int> center(7), tmp(7);

  fin = fopen("n3ssmi.20040612","r");
  x.binin(fin);
  fclose(fin);
  for (i = 0; i < y.xpoints(); i++) {
    y[i].resize(7);
  }
  count = 0;

  tmax.obs.t19v = 0;
  tmax.obs.t19h = 0;
  tmax.obs.t22v = 0;
  tmax.obs.t37v = 0;
  tmax.obs.t37h = 0;
  tmax.obs.t85v = 0;
  tmax.obs.t85h = 0;
  tmin.obs.t19v = 50000;
  tmin.obs.t19h = 50000;
  tmin.obs.t22v = 50000;
  tmin.obs.t37v = 50000;
  tmin.obs.t37h = 50000;
  tmin.obs.t85v = 50000;
  tmin.obs.t85h = 50000;

  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    tmax.obs.t19v = max(tmax.obs.t19v, x[loc].obs.t19v);
    tmax.obs.t19h = max(tmax.obs.t19h, x[loc].obs.t19h);
    tmax.obs.t22v = max(tmax.obs.t22v, x[loc].obs.t22v);
    tmax.obs.t37v = max(tmax.obs.t37v, x[loc].obs.t37v);
    tmax.obs.t37h = max(tmax.obs.t37h, x[loc].obs.t37h);
    tmax.obs.t85v = max(tmax.obs.t85v, x[loc].obs.t85v);
    tmax.obs.t85h = max(tmax.obs.t85h, x[loc].obs.t85h);
    if (x[loc].obs.t19v != 0) tmin.obs.t19v = min(tmin.obs.t19v, x[loc].obs.t19v);
    if (x[loc].obs.t19h != 0) tmin.obs.t19h = min(tmin.obs.t19h, x[loc].obs.t19h);
    if (x[loc].obs.t22v != 0) tmin.obs.t22v = min(tmin.obs.t22v, x[loc].obs.t22v);
    if (x[loc].obs.t37v != 0) tmin.obs.t37v = min(tmin.obs.t37v, x[loc].obs.t37v);
    if (x[loc].obs.t37h != 0) tmin.obs.t37h = min(tmin.obs.t37h, x[loc].obs.t37h);
    if (x[loc].obs.t85v != 0) tmin.obs.t85v = min(tmin.obs.t85v, x[loc].obs.t85v);
    if (x[loc].obs.t85h != 0) tmin.obs.t85h = min(tmin.obs.t85h, x[loc].obs.t85h);
// If a nonzero point, add to vector
    if ( (x[loc].obs.t19v != 0) &&  (x[loc].obs.t19h != 0) &&  (x[loc].obs.t22v != 0) &&  (x[loc].obs.t37v != 0) &&  (x[loc].obs.t37h != 0) &&  (x[loc].obs.t85v != 0) &&  (x[loc].obs.t85h != 0) ) {
      count += 1;
      y[count][0] = x[loc].obs.t19v;
      y[count][1] = x[loc].obs.t19h;
      y[count][2] = x[loc].obs.t22v;
      y[count][3] = x[loc].obs.t37v;
      y[count][4] = x[loc].obs.t37h;
      y[count][5] = x[loc].obs.t85v;
      y[count][6] = x[loc].obs.t85h;
    } 

  }
  }

  printf("tmax: \n");
  printf("%d %d %d %d %d %d %d\n",tmax.obs.t19v, tmax.obs.t19h, tmax.obs.t22v, 
tmax.obs.t37v, tmax.obs.t37h, tmax.obs.t85v, tmax.obs.t85h);
  printf("tmin: \n");
  printf("%d %d %d %d %d %d %d\n",tmin.obs.t19v, tmin.obs.t19h, tmin.obs.t22v, 
tmin.obs.t37v, tmin.obs.t37h, tmin.obs.t85v, tmin.obs.t85h);

  printf("count = %d\n",count);

  center[0] = 200*100;
  center[1] = 200*100;
  center[2] = 200*100;
  center[3] = 200*100;
  center[4] = 200*100;
  center[5] = 200*100;
  center[6] = 200*100;
  for (i = 0; i < count; i++) {
    tmp = center;
    tmp -= y[i];
    if (tmp.norm() < radius) {
      recount += 1;
    }
  }
  printf("count = %d,  recount = %d\n",count, recount); 

  return 0;
}
