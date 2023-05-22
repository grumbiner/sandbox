#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  northgrid<float> tshal, sshal, tdeep, sdeep;
  ijpt loc;

  fin = fopen(argv[1], "r");
  tshal.ftnin(fin);
  //tshal.printer(stdout);
  //printf("Now for salinity\n");
  sshal.ftnin(fin);
  //sshal.printer(stdout);
  fclose(fin);

  fin = fopen(argv[2], "r");
  tdeep.ftnin(fin);
  //tdeep.printer(stdout);
  //printf("Now for salinity\n");
  sdeep.ftnin(fin);
  //sdeep.printer(stdout);
  fclose(fin);

  for (loc.j = 0; loc.j < tshal.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < sshal.xpoints(); loc.i++) {
     printf("%3d %3d  %5.2f %5.2f %f  %5.2f %5.2f %f\n", loc.i, loc.j,
            tshal[loc], tdeep[loc], tshal[loc] - tdeep[loc],
            sshal[loc], sdeep[loc], sshal[loc] - sdeep[loc] );
  }
  }



  return 0;
} 
