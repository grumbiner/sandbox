#include <stdio.h>

#include "ncepgrids.h"

// Compute the real albedo over the ice pack given the shortwave fluxes 
// Robert Grumbine 7 October 1998

class gaussian : public grid2<float> { 
  public:
    gaussian();
    gaussian(int);
};
gaussian::gaussian() {
  nx = 0; ny= 0;
  grid = (float *) malloc(nx * ny * sizeof(float) );
}
gaussian::gaussian(int t) {
  nx = 3*t + 6;
  ny = (3*t + 2 ) / 2;
  grid = (float *) malloc(nx * ny * sizeof(float) );
}

int main(int argc, char *argv[]) {
  gaussian albdo(126), swdown(126), swup(126), icec(126), inf1(126);
  FILE *fina, *find, *finu, *finice, *fout;
  ijpt x;

  if (argc < 6) {
    printf("need 5 arguments\n");
    return -1;
  }
  fina = fopen(argv[1],"r");
  find = fopen(argv[2],"r");
  finu = fopen(argv[3],"r");
  finice = fopen(argv[4],"r");
  fout = fopen(argv[5],"w");
  if (fina == (FILE *) NULL || find == (FILE*) NULL || finu == (FILE*) NULL 
       || finice == (FILE *) NULL || fout == (FILE*) NULL) {
    printf("failed to open a needed file\n");
    return -1;
  }

  albdo.binin(fina);
  swdown.binin(find);
  swup.binin(finu);
  icec.binin(finice);

  for (x.j = 0; x.j < albdo.ypoints() ; x.j++) {
    for (x.i = 0; x.i < albdo.xpoints() ; x.i++) {
       if (swdown[x] > 0.) {
         inf1[x] = swup[x] / swdown[x];
       }
       else {
         inf1[x] = -1./128.;
       }
// Selective print now, at least 10 W/m^2 downwelling, and an ice cover
       if (swdown[x] > 10.0 && icec[x] > 0.45) {
         fprintf(fout, "%3d %3d  %5.1f %4.0f %4.0f  %5.1f %5.1f \n",x.i, x.j, 
            albdo[x],
            swup[x], swdown[x], 100.*icec[x], 100.*inf1[x] );
       }
    }
  }

  return 0;
}
