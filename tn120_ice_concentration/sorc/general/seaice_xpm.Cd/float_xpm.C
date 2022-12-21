#include <stdio.h>

// This is a generalized program to construct sea ice xpm files
//   for any known grid (GRIDTYPE).  GRIDTYPE must be specified in compilation.
// Arguments are: ice_data land_file output_file
// Robert Grumbine 
// Doc: 9 December 2002

#include "ncepgrids.h"


int main(int argc, char *argv[]) {
  FILE *fin;
  GRIDTYPE<float> in, yland; 
  GRIDTYPE<unsigned char> landin;
  palette<unsigned char> gg(19, 65);

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
  landin.binin(fin); 
  fclose(fin);
  conv(landin, yland);

  in.colorproc(yland, 7, 65, std_ice_coloring);
  in.xpm(argv[3], 1, gg); 

  return 0;
}
