#include <time.h>
#include "ncepgrids.h"

// Illustration of using the color processing function to produce
//   xpm output files
// Robert Grumbine 28 June 1999

int main(void) {
  northgrid<unsigned char> y, yland;
  unsigned char tmp1, tmp2;
  FILE *fin;
  ijpt loc;
  int k;
  palette<unsigned char> gg(19, 65);

  fin = fopen("north", "r");
  y.binin(fin);
  fclose(fin);
  fin = fopen("nland.map", "r");
  yland.binin(fin);
  fclose(fin);

  y.colorproc(yland, 7, 65, &std_ice_coloring);
  y.xpm("n.xpm", 1, gg);

  return 0;
}

