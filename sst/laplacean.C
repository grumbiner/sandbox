#include "ncepgrids.h"

void fill(global_ice<float> &x, float toler, int itmax);

int main(void) {
  global_ice<float> sst2d, rtglow, downavg, area, delta;
  global_12th<float> rtghigh;
  global_12th<unsigned char> land;
  ijpt loc, tloc;
  latpt ll;
  FILE *fin;
  float toler = 0.0;

  fin = fopen("sst2dvar.bin","r");
  sst2d.binin(fin);
  fclose(fin);
  fin = fopen("rtglow.bin","r");
  rtglow.binin(fin);
  fclose(fin);
  fin = fopen("rtghigh.bin","r");
  rtghigh.binin(fin);
  fclose(fin);
  fin = fopen("seaice_gland5min","r");
  land.binin(fin);
  fclose(fin);

  area.set((float) 0.0);
  downavg.set((float) 0.0);
  for (loc.j = 0; loc.j < rtghigh.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < rtghigh.xpoints(); loc.i++) {
    if (land[loc] == 0) {
      ll = rtghigh.locate(loc);
      tloc = area.locate(ll);
      area[tloc]    += rtghigh.cellarea(loc);
      downavg[tloc] += rtghigh[loc]*rtghigh.cellarea(loc);
    }
  }
  }

  global_ice<unsigned char> mask;
  mask.set(0);
  for (loc.j = 0; loc.j < delta.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < delta.xpoints(); loc.i++) {
    if (area[loc] != 0) {
      downavg[loc] /= area[loc]; 
      mask[loc] = 1;
    }
  }
  }

  fill(downavg, 0.01, 5000);
  delta = downavg; delta -= sst2d;
  for (loc.j = 0; loc.j < delta.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < delta.xpoints(); loc.i++) {
    ll = delta.locate(loc);
    if (fabs(delta[loc]) > 0.5 && mask[loc] == 0) {
      printf("%3d %3d  %7.2f %7.2f  %6.2f %6.2f %6.2f  %6.2f %6.2f %6.2f\n",
            loc.i, loc.j, ll.lon, ll.lat, rtglow[loc], sst2d[loc], 
            downavg[loc], delta[loc], rtglow[loc] - sst2d[loc], downavg[loc] - rtglow[loc]);
    }
  }
  }


  palette<unsigned char> gg(19, 65);
  sst2d.scale(271, 320);
  rtglow.scale(271, 320);
  downavg.scale(271, 320);
  sst2d.xpm("sst2d.xpm",7,gg);
  rtglow.xpm("rtglow.xpm",7,gg);
  downavg.xpm("rtgdown.xpm",7,gg);

  printf("delta: %f %f %f\n",delta.gridmax(), delta.gridmin(), delta.average() );
  delta.scale(delta.gridmin(), delta.gridmax()) ;
  delta.xpm("delta.xpm",7,gg);

  return 0;
} 
void fill(global_ice<float> &x, float toler, int itmax) {
  global_ice<unsigned char> mask;
  global_ice<float> y;
  ijpt loc, ip, jp, im, jm;
  int mask_count = 0, iter;
  float maxdel = 0, avg, del;
  y = x;
  avg = x.average(0.0);
  printf("avg = %f\n",avg); fflush(stdout);
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (x[loc] == 0) { 
      mask[loc] = 0; 
      y[loc] = 271.35;
      mask_count++;
    }
    else {
      mask[loc] = 1;
    }
  }
  }   
  printf("mask count %d\n",mask_count);

  iter = 0;
  float sor = 1.5;
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
    printf("iter %d maxdel %f\n",iter, maxdel); 
    iter++;
  }

  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (x[loc] != y[loc] && x[loc] != 0) {
      printf("fill %3d %3d  %f %f  %f\n",loc.i, loc.j, x[loc], y[loc], x[loc]-y[loc]);
    }
  }
  }
  x = y;

  return;
}
