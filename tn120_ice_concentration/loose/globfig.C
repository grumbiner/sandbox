#include <stdio.h>

#include "ncepgrids.h"

#define LAND 157
#define COAST 195
#define WEATHER 177
#define NO_DATA 224
#define MAXICE  127

template <class T>
void xpmice(metricgrid<T> &newice, char *fname) ;

int main(int argc, char *argv[]) {
  global_ice<unsigned char> globice;
  FILE *nin;
  int diff_count, tag;
  char fname[90];
  ijpt x;
  latpt y;
  palette<unsigned char> gg(19, 65);

  nin = fopen(argv[1], "r");
  globice.binin(nin);
  fclose(nin);

  printf("glob ice max, min, average %d %d %d\n",
          (int) globice.gridmax(), (int) globice.gridmin(), 
          (int) globice.average() 
         );

// Ensure that the scaling is the same (0-100.)
  if ( (float) globice.average() < 3.0 ) {
     globice *= 100;
     printf("rescaling \n");
  }
  
  sprintf(fname,"%s.xpm",argv[1]);
  xpmice(globice, fname);

  return 0;

}
template <class T>
void xpmice(metricgrid<T> &newice, char *fname) {
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
         newice[x] = 0;
       }
       else if (newice[x] > MAXICE) {
         newice[x] = 2;
       }
       else {
         newice[x] = 4 + min((unsigned char)100,newice[x])/7;
       }
    }
  }
  newice.xpm(&fname[0], 1, gg);

  return ;
}
