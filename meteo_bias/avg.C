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
  gaussian_t126 netrad, netsurf, netmelt, netheat, tempor;
  gaussian_t126 icec, albdo, land;
  ijpt xij;

  int i, j, k, yr, mo, dy, hr;
  float count;
  float rho = 1000., cp = 4.e3, lf = 3.32e5;
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

  count = 0.0;
  for (mo = 5; mo <  9; mo++) {
    int dystart, dyend;
    if ( mo == 5 ) {
      dystart = 15;
      dyend   = 31;
    }
    else if (mo == 9) {
      dystart = 1;
      dyend   = 15;
    }
    else {
      dystart = 1;
      dyend   = 31;
    }

  for (dy = dystart; dy < dyend; dy++) {
    //printf("dy = %02d\n",dy);
    for (i = 0; i < 4; i++) {
      sprintf(fname, "SHTFL.%02d%02d%02d%02d",yr, mo, dy, i*6);
      fin = fopen(fname, "r");
      if (fin == NULL) {
        continue;
      }
      count += 1.0;
      tempor.binin(fin);
      fclose(fin);
      sh += tempor;

      sprintf(fname, "LHTFL.%02d%02d%02d%02d",yr, mo, dy, i*6);
      fin = fopen(fname, "r");
      tempor.binin(fin);
      fclose(fin);
      lh += tempor;

      sprintf(fname, "DLWRF.%02d%02d%02d%02d",yr, mo, dy, i*6);
      fin = fopen(fname, "r");
      tempor.binin(fin);
      fclose(fin);
      lwdown += tempor;

      sprintf(fname, "ULWRF.%02d%02d%02d%02d",yr, mo, dy, i*6);
      fin = fopen(fname, "r");
      tempor.binin(fin);
      fclose(fin);
      lwup += tempor;

      sprintf(fname, "USWRF.%02d%02d%02d%02d",yr, mo, dy, i*6);
      fin = fopen(fname, "r");
      tempor.binin(fin);
      fclose(fin);
      swup += tempor;

      sprintf(fname, "DSWRF.%02d%02d%02d%02d",yr, mo, dy, i*6);
      fin = fopen(fname, "r");
      tempor.binin(fin);
      fclose(fin);
      swdown += tempor;

      sprintf(fname, "ALBDO.%02d%02d%02d%02d",yr, mo, dy, i*6);
      fin = fopen(fname, "r");
      tempor.binin(fin);
      fclose(fin);
      albdo += tempor;

      sprintf(fname, "ICEC.%02d%02d%02d%02d",yr, mo, dy, i*6);
      fin = fopen(fname, "r");
      tempor.binin(fin);
      fclose(fin);
      icec += tempor;

      sprintf(fname, "LAND.%02d%02d%02d%02d",yr, mo, dy, i*6);
      fin = fopen(fname, "r");
      tempor.binin(fin);
      fclose(fin);
      land += tempor;

    }
  }
  } // done looping over months

  printf(" count = %f\n",count);
  sh /= count;
  lh /= count;
  lwdown /= count;
  lwup /= count;
  swup /= count;
  swdown /= count;

  icec /= count;
  land /= count;
  albdo /= count;
//  Now have daily averaged fluxes 
  for (xij.j = 0; xij.j < netrad.ypoints() / 4; xij.j++) {
  for (xij.i = 0; xij.i < netrad.xpoints()    ; xij.i++) {
    if (icec[xij] > 0 || land[xij] < 0.5 ) {
      netrad.grid[xij.i + xij.j * netrad.xpoints() ] = 
        lwdown[xij] - lwup[xij] + swdown[xij] - swup[xij];
      netsurf.grid[xij.i + xij.j * netsurf.xpoints() ] = 
        sh[xij] + lh[xij];
      printf("%3d %3d %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f  %6.1f %6.2f %2.0f %1.0f\n",
            xij.i, xij.j, sh[xij], lh[xij], 
            lwup[xij], lwdown[xij], swup[xij], swdown[xij],
            netsurf[xij] - netrad[xij], icec[xij], albdo[xij], land[xij] );
     }
  }
  }

  return 0;

}
