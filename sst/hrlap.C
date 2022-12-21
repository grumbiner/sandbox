#include "ncepgrids.h"

void fill(global_12th<float> &x, global_12th<unsigned char> &mask, float toler, int itmax);

int main(void) {
  global_12th<float> rtghigh, newer, delta;
  global_12th<unsigned char> land, mask;
  ijpt loc, tloc;
  latpt ll;
  FILE *fin;
  float toler = 5.0;

  fin = fopen("rtghigh.bin","r");
  rtghigh.binin(fin);
  fclose(fin);
  fin = fopen("seaice_gland5min","r");
  land.binin(fin);
  fclose(fin);

  mask.set(0);
  newer = rtghigh;
  float lowest = rtghigh.gridmin();

  for (loc.j = 0; loc.j < rtghigh.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < rtghigh.xpoints(); loc.i++) {
    if (land[loc] == 0) {
      mask[loc] = 1;
    }
    if (rtghigh[loc] > lowest + 40.95) {
      ll = rtghigh.locate(loc);
      printf("exceptional %4d %4d %8.3f %8.3f  %7.2f %3d\n",loc.i, loc.j, ll.lon, ll.lat,
        rtghigh[loc], land[loc]);
    }
  }
  }
  //return 0;

  fill(newer, mask, 0.01, 5000);

  delta = newer; delta -= rtghigh;
  for (loc.j = 0; loc.j < delta.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < delta.xpoints(); loc.i++) {
    if (fabs(delta[loc]) > toler ) {
      ll = delta.locate(loc);
      printf("%4d %4d  %8.3f %8.3f  %6.2f %6.2f %6.2f  %3d\n",
            loc.i, loc.j, ll.lon, ll.lat, rtghigh[loc], newer[loc], delta[loc], land[loc] );
    }
  }
  }

  palette<unsigned char> gg(19, 65);
  printf("delta: %f %f %f\n",delta.gridmax(), delta.gridmin(), delta.average() );
  delta.scale(delta.gridmin(), delta.gridmax()) ;
  delta.xpm("delta.xpm",7,gg);

  return 0;
} 
void fill(global_12th<float> &x, global_12th<unsigned char> &mask, float toler, int itmax) {
  global_12th<float> y;
  ijpt loc, ip, jp, im, jm;
  int mask_count = 0, iter;
  float maxdel = 0, del;
  float sor = 1.5;

  y = x;

  iter = 0;
  maxdel = 9;
  while (iter < itmax && maxdel > toler) {
    maxdel = 0;
    for (loc.j = 1; loc.j < x.ypoints()-1; loc.j++) {
    ip.j = loc.j;
    im.j = loc.j;
    jp.j = loc.j + 1;
    jm.j = loc.j - 1;
    for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
      if (mask[loc] == 0 ) {
        ip.i = loc.i+1; if (ip.i == x.xpoints()) { ip.i = 0;}
        im.i = loc.i-1; if (ip.i == -1) { ip.i = x.xpoints() - 1; }
        jp.i = loc.i;
        jm.i = loc.i;
        del = sor*(-y[loc] + 0.25*(y[ip]+y[jp]+y[im]+y[jm])); 
        maxdel = max(fabs(del), maxdel);
        y[loc] += del;
       }
    }
    }
    printf("iter %d maxdel %f\n",iter, maxdel); fflush(stdout);
    iter++;
  }

// verify that no water points (mask == 1) were changed
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (x[loc] != y[loc] && mask[loc] == 1) {
      printf("fill %3d %3d  %f %f  %f\n",loc.i, loc.j, x[loc], y[loc], x[loc]-y[loc]);
    }
  }
  }
  x = y;

  return;
}
