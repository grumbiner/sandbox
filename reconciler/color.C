#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  northhigh<int> count;
  northhigh<unsigned char> mask, scale;
  ijpt loc;
  palette<unsigned char> gg(19,65);
  int days;

  fin = fopen(argv[1],"r");
  count.binin(fin);
  fclose(fin);
  fin = fopen(argv[2],"r");
  mask.binin(fin);
  fclose(fin);

  days = count.gridmax();
  count *= 128;
  for (loc.j = 0 ; loc.j < mask.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < mask.xpoints(); loc.i++) {
    scale[loc] = count[loc]/days;
    if (mask[loc] == 157) {
      scale[loc] = 0;
    }
  }
  }
  gg.invert();
  scale.xpm(argv[3],7,gg);

  return 0;

} 
