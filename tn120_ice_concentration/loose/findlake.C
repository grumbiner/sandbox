#include <stdio.h>

#include "ncepgrids.h"
#define LAND 157
#define COAST 195

int main(int argc, char *argv[] ) {
  FILE *fin;
  northgrid<unsigned char> nland;
  latpt loc;
  ijpt y,x; 
  fijpt fy;

  fin = fopen("nland.map", "r");
  nland.binin(fin);
  loc.lat = atof(argv[1]);
  loc.lon = atof(argv[2]);
  fy = nland.locate(loc);

  for (x.j = fy.j - 5; x.j < fy.j + 5; x.j++) {
  for (x.i = fy.i - 5; x.i < fy.i + 5; x.i++) {
    if (nland[x] == LAND) {
      printf("L");
    }
    else if (nland[x] == COAST) {
      printf("C");
    }
    else {
      printf("0");
    }
  }
  printf("\n");
  }

  return 0;
}
  
  
