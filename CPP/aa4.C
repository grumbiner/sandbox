#include <stdlib.h>
#include "ncepgrids.h"

#define MINUTES 0.5
int main(int argc, char *argv[]) {
  llgrid<float> dest((360*60)/MINUTES+1, ((-60- -90)*60)/MINUTES+1, 
                       MINUTES/60., MINUTES/60., -90, 0.0);
  FILE *fin, *fout;
  float flagval = -1, nonval = -99;
  ijpt loc, l2;
  latpt ll;
  palette<unsigned char> gg(19,65);

  printf("llgrid nx, ny = %d %d\n",dest.xpoints(), dest.ypoints() );

  return 0;
}

