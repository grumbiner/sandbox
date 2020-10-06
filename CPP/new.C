#include <stdio.h>
#include <stdlib.h>

#include "eta.h"

int main(int argc, char *argv[]) {
  eta32<float> x, etaland;
  llgrid<float> y(360, 180, 1.0, 1.0, -89.5, -179.5);
  llgrid<float> fland(360, 180, 1.0, 1.0, -89.5, -179.5);
  llgrid<unsigned char> yland(360, 180, -1.0, 1.0, +89.5, -179.5);

  ijpt loc, nloc, lower, upper;
  latpt ll;
  float flag = -1.0;
  FILE *fin, *fout, *gl;
  int yy, mm, dd, hh=0;
  int parmno=91, depth=0, lead=0, mxbit=8;
  char *grib;
  int lgrib = 0, ierr = 0;
  int i;


// Now start some attempts to work:
  fin = fopen("etaland", "r");
  etaland.binin(fin);
  fclose(fin);

  fin = fopen("onedeg.map", "r");
  yland.binin(fin);
  fclose(fin);

  yy = atoi(argv[3]);
  mm = atoi(argv[4]);
  dd = atoi(argv[5]);

  flag = 157.;
  fin = fopen(argv[1], "r");
  y.binin(fin);
  printf("y max, min, avg = %f %f %f\n",y.gridmax(), y.gridmin(), y.average());
  for (loc.j = 0; loc.j < y.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < y.xpoints(); loc.i++) {
     if (y[loc] > 200.) y[loc] = flag;
  }
  }
  fflush(stdout);
  y *= 100;
  fclose(fin);
  fflush(stdout);
 
  for (loc.j = 0; loc.j < y.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < y.xpoints(); loc.i++) {
    nloc.i = loc.i;
    nloc.j = yland.ypoints() - loc.j - 1; 
    fland[nloc] = (float) yland[loc];
    if (fland[nloc] == 195.) fland[nloc] = flag;
    if (y[loc] > 200.) y[loc] = flag;
  }
  }
  printf("zzzzzzzzz \n");

// Print out eta land mask:
  //lower.i = 132;
  //lower.j = 143;
  //upper.i = 190;
  //upper.j = 236;
  //printf("%3d %3d  %3d %3d\n",lower.i, lower.j, upper.i, upper.j);
  //for (loc.j = upper.j; loc.j > lower.j; loc.j--) {
  //for (loc.i = lower.i; loc.i < upper.i; loc.i++) {
  ////for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  ////for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
  //  ll = etaland.locate(loc);
  //  printf("%3d %3d  %f %f  %1d \n",loc.i, loc.j, ll.lat, ll.lon, 
  //             (int)etaland[loc]);
  //}
  //}
  //printf("zzzzzzzzz \n");


// Perform the interpolation
  x.fromall(y, fland, flag, 224.);

// Loop back through and perform qc checks
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (x[loc] > 128) {
      if (x[loc] != 224) printf("reset2 %3d %3d  %f\n",loc.i, loc.j,  x[loc]);
      x[loc] = 0;
    }
    if (etaland[loc] == 1 && x[loc] != 0) {
      //ll = x.locate(loc);
      //printf("reset1 %3d %3d  %4.1f %5.1f  %5.1f %3.1f\n",loc.i, loc.j, ll.lat, ll.lon, x[loc], etaland[loc]);  
      x[loc] = 0;
    }
    ll = x.locate(loc);
    printf("%3d %3d  %4.1f %5.1f  %5.1f %3.1f\n",loc.i, loc.j, ll.lat, ll.lon, x[loc], etaland[loc]);  
  }
  }

// Write back out:
  printf("average = %f %f %f\n",x.average(), x.gridmin(), x.gridmax() );
  fout = fopen(argv[2], "w");


  x /= 100.; // rescale to percents
  x.pds.set_time(yy, mm, dd, hh);
  x.pds.set_precision(0.01);
  grib = new char [(x.xpoints() * x.ypoints() * mxbit)/8 + 200];
  if (grib == NULL) {
    printf("failed to new the grib field\n");
  }
  //ierr = x.gribit(parmno, depth, lead, grib, lgrib, mxbit);
  //if (ierr != 0) {
  //  printf("Error %d in constructing grib file!\n",ierr);
  //}
  //else {
  //  i = fwrite(grib, sizeof(char), lgrib, fout);
  //}
  //printf("Wrote out %d of %d\n", i, lgrib);

  #ifdef IBM
  x.ftnout(fout);
  #else
  x.binout(fout);
  #endif
  fclose(fout);

  return 0;
}
