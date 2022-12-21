#include <stdio.h>

#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  GRIDTYPE<DATTYPE> in, yland; 
  GRIDTYPE<unsigned char> land;
  palette<unsigned char> gg(19, 65);

  fin = fopen(argv[1],"r");
  if (fin == (FILE *) NULL ) {
    printf("failed to open data file\n");
    return 1;
  }
  in.binin(fin);
  fclose(fin);
  fin = fopen(argv[2], "r");
  if (fin == (FILE *) NULL ) {
    printf("failed to open land file\n");
    return 1;
  }
  land.binin(fin); 
  fclose(fin);

  if (in.average() < 5) in *= 100.;
  conv(land, yland);
  in.colorproc(yland, 7, 65, std_ice_coloring);
  in.xpm(argv[3], 1, gg); 

  return 0;
}
