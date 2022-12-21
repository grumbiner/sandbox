#include <stdio.h>

#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  GRIDTYPE<DATTYPE> in, yland; 
  GRIDTYPE<unsigned char> land;
  palette<unsigned char> gg(19, 65);

  fin = fopen(argv[1],"r");
  if (fin == (FILE *) NULL ) {
    printf("failed to open input data file\n");
    return 1;
  }
  in.binin(fin);
  fclose(fin);
  fin = fopen(argv[2], "r");
  if (fin == (FILE *) NULL ) {
    printf("failed to open input land file\n");
    return 1;
  }
  yland.binin(fin); 
  fclose(fin);

  in += M_PI;
  in *= 180. / M_PI;
  printf("in max, min, avg %f %f %f\n",in.gridmax(), in.gridmin(), in.average() );
  printf("yland max, min, avg %f %f %f\n",yland.gridmax(), yland.gridmin(), yland.average() );
  in /= 3.0;
  if (yland.gridmax() < 3.) land *= 100.;
  in.colorproc(yland, 7, 65, std_ice_coloring);
  in.xpm(argv[3], 7, yland, gg); 

  return 0;
}
