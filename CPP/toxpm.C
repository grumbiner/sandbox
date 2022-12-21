#include <stdio.h>

// This is a generalized program to construct sea ice xpm files
//   for any known grid (GRIDTYPE) and with any input data type
//   DATTYPE.  GRIDTYPE and DATTYPE must be specified in compilation.
// It is assumed, however, that the land file is unsigned character.
// Argumens are: ice_data land_file output_file
// Robert Grumbine 
// Doc: 9 December 2002
// Accept the land mask flag value as the 4th argument

#include <stdlib.h>
#include "ncepgrids.h"
#include "eta.h"

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  GRIDTYPE<DATTYPE> in, yland; 
  GRIDTYPE<float> land;
  palette<unsigned char> gg(19, 65);
  ijpt loc;
  unsigned char flag;

  fin = fopen(argv[1],"r");
  if (fin == (FILE *) NULL ) {
    printf("failed to open input file\n");
    return 1;
  }
  in.binin(fin);
  fclose(fin);
  if (in.average() < 3) in *= 100;
 
  fin = fopen(argv[2], "r");
  if (fin == (FILE *) NULL ) {
    printf("failed to open land file\n");
    return 1;
  }
  land.binin(fin); 
  fclose(fin);
  conv(land, yland);

  flag = atoi(argv[4]);
  if (flag != (unsigned char) 157) {
    for (loc.j = 0; loc.j < yland.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < yland.xpoints(); loc.i++) {
       if (yland[loc] == flag) yland[loc] = (unsigned char) 157;
    }
    }
  }

  in.colorproc(yland, 7, 65, std_ice_coloring);
  in.xpm(argv[3], 1, gg); 

  return 0;
}
