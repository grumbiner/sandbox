#include "ncepgrids.h"

int main(void) {
  northhigh<unsigned char> nland, nconc;
  FILE *fin;
  palette<unsigned char> gg(19, 65);

  fin = fopen("nland.new", "r");
  nland.binin(fin);
  fclose(fin);

  fin = fopen("nconc", "r");
  nconc.binin(fin);
  fclose(fin);

  nconc.xpm("nh.xpm", nland, 12, gg);

  return 0;
}
