#include <stdio.h>

#include "ncepgrids.h"

#define LAND 157
#define COAST 195
#define WEATHER 177
#define NO_DATA 224
#define MAXICE  127

void xpmice(metricgrid<float> &newice, char *fname) ;

int main(int argc, char *argv[]) {
  northgrid<float> nhice;
  southgrid<float> shice;
  FILE *nin, *sin;
  int diff_count, tag;
  char fname[90];
  ijpt x;
  latpt y;
  palette<unsigned char> gg(19, 65);

  nin = fopen(argv[1], "r");
  sin = fopen(argv[2], "r");
  nhice.binin(nin);
  shice.binin(sin);
  fclose(nin);
  fclose(sin);

  printf("nh ice max, min, average %f %f %f\n",nhice.gridmax(), nhice.gridmin(), nhice.average() );
  printf("sh ice max, min, average %f %f %f\n",shice.gridmax(), shice.gridmin(), shice.average() );

// Ensure that the scaling is the same (0-100.)
  if (nhice.average() < 3.0 ) nhice *= 100.;
  if (shice.average() < 3.0 ) shice *= 100.;
  
  sprintf(fname,"%s.xpm",argv[1]);
  xpmice(nhice, fname);
  sprintf(fname,"%s.xpm",argv[2]);
  xpmice(shice, fname);

  return 0;

}
void xpmice(metricgrid<float> &newice, char *fname) {
  ijpt x;
  latpt y;
  palette<unsigned char> gg(19, 65);
// Print out xpm files for graphic comparisons
  for (x.j = 0; x.j < newice.ypoints() ; x.j++) {
    for (x.i = 0; x.i < newice.xpoints() ; x.i++) {
//       if (land[x] == LAND) {
//         newice[x] = 0;
//       }
//       else if (land[x] == COAST) {
//         newice[x] = 0;
//       }
       if (newice[x] == NO_DATA) {
         newice[x] = 2;
       }
       else if (newice[x] > MAXICE) {
         newice[x] = 3;
       }
       else {
         newice[x] = 4 + min((float) 100,newice[x])/7;
       }
    }
  }
  newice.xpm(&fname[0], 1, gg);

  return ;
}
