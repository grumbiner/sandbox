#include <stdio.h>
#include <malloc.h>
#include <stdlib.h>

#include "grid_math.h"
#include "points.h"

class gaussian_t126 : public grid2<float> {
  public:
   gaussian_t126();
   ~gaussian_t126();
};
gaussian_t126::gaussian_t126() {
  nx = 384;
  ny = 190;
  grid = (float *) malloc(nx*ny*sizeof(float) );
}
gaussian_t126::~gaussian_t126() {
  nx = 0;
  ny = 0;
}

int main(int argc, char *argv[]) {
  gaussian_t126 sh, lh, lwdown, swdown, lwup, swup;
  gaussian_t126 netrad, netsurf, netmelt, netfreez,netheat, tempor;
  gaussian_t126 icec, albdo, land;
  ijpt xij;

  int i, j, k, yr, mo, dy, hr;
  float count, icecount, ocecount;
  float rho = 1000., cp = 4.e3, lf = 3.32e5, hmix = 25.0;
  char fname[80];
  FILE *fin;

  i = atoi(argv[1]);
  
  yr = i/100;
  mo = i % 100 ;

  sh.set(0.0);
  lh.set(0.0);
  lwdown.set(0.0);
  lwup.set(0.0);
  swup.set(0.0);
  swdown.set(0.0);
  netmelt.set(0.0);
  netfreez.set(0.0);
  netrad.set(0.0);
  netsurf.set(0.0);
  netheat.set(0.0);

  count = 0.0;
  for (dy = 1; dy < 31; dy++) {
    //printf("yr mo dy = %02d %02d %02d\n",yr, mo, dy); fflush(stdout);
    for (i = 0; i < 4; i++) {
      netsurf.set(0.0);
      netrad.set(0.0);

      sprintf(fname, "SHTFL.%02d%02d%02d%02d",yr, mo, dy, i*6);
      fin = fopen(fname, "r");
      if (fin == NULL) {
        continue;
      }
      count += 1.0;
      sh.binin(fin);
      fclose(fin);
      //printf("sh avg = %f\n", sh.average() ); fflush(stdout);

      sprintf(fname, "LHTFL.%02d%02d%02d%02d",yr, mo, dy, i*6);
      fin = fopen(fname, "r");
      lh.binin(fin);
      fclose(fin);

      sprintf(fname, "DLWRF.%02d%02d%02d%02d",yr, mo, dy, i*6);
      fin = fopen(fname, "r");
      lwdown.binin(fin);
      fclose(fin);

      sprintf(fname, "ULWRF.%02d%02d%02d%02d",yr, mo, dy, i*6);
      fin = fopen(fname, "r");
      lwup.binin(fin);
      fclose(fin);

      sprintf(fname, "USWRF.%02d%02d%02d%02d",yr, mo, dy, i*6);
      fin = fopen(fname, "r");
      swup.binin(fin);
      fclose(fin);

      sprintf(fname, "DSWRF.%02d%02d%02d%02d",yr, mo, dy, i*6);
      fin = fopen(fname, "r");
      swdown.binin(fin);
      fclose(fin);

      sprintf(fname, "ALBDO.%02d%02d%02d%02d",yr, mo, dy, i*6);
      fin = fopen(fname, "r");
      albdo.binin(fin);
      fclose(fin);

      sprintf(fname, "ICEC.%02d%02d%02d%02d",yr, mo, dy, i*6);
      fin = fopen(fname, "r");
      icec.binin(fin);
      fclose(fin);

      sprintf(fname, "LAND.%02d%02d%02d%02d",yr, mo, dy, i*6);
      fin = fopen(fname, "r");
      land.binin(fin);
      fclose(fin);

      for (xij.j = 0; xij.j < netrad.ypoints() ; xij.j++) {
      for (xij.i = 0; xij.i < netrad.xpoints() ; xij.i++) {
        netsurf.grid[xij.i + xij.j*netsurf.xpoints()] += 
          sh[xij] + lh[xij] - 
          (lwdown[xij] - lwup[xij] + swdown[xij] - swup[xij]) ;
        if (icec[xij] > 0 && netsurf[xij] < 0) { 
          netmelt.grid[xij.i + xij.j*netmelt.xpoints()] += 
                           netsurf[xij] / (rho*lf) * 6. * 3600.;
        }
        else if (icec[xij] > 0 && netsurf[xij] > 0) {
          netfreez.grid[xij.i + xij.j*netmelt.xpoints()] += 
                           netsurf[xij] / (rho*lf) * 6. * 3600.;
        } 
        else {
          netheat.grid[xij.i + xij.j*netmelt.xpoints()] -= 
                           netsurf[xij] / (rho*cp*hmix) * 6. * 3600.;
        } 
      }
      }
      //printf("netsurf average %f\n", netsurf.average() ); fflush(stdout);

    }
  }

//  Now have integrated equivalent fluxes.  Melt in meters, heat in degrees
  for (xij.j = 0; xij.j < netrad.ypoints() ; xij.j++) {
  for (xij.i = 0; xij.i < netrad.xpoints() ; xij.i++) {
    printf("%3d %3d  %5.2f %5.2f %6.2f \n", xij.i, xij.j, 
           netmelt[xij], netfreez[xij], netheat[xij]);
  }
  }

  return 0;

}
