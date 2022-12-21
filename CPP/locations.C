#include <stdlib.h>
#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  fijpt ll, ur;
  ijpt ill, iur;
  latpt loc1, loc2;
  northhigh<unsigned char> north;
  southhigh<unsigned char> south;
  psgrid<unsigned char> tmp;
  bool ok;
  palette<unsigned char> gg(19, 65);
  FILE *fin;

  fin = fopen("nland.new", "r");
  north.binin(fin);
  fclose(fin);
  fin = fopen("sland.new", "r");
  south.binin(fin);
  fclose(fin);

  ill.i = atoi(argv[1]);
  ill.j = atoi(argv[2]);
  iur.i = atoi(argv[3]);
  iur.j = atoi(argv[4]);

  tmp.subset(north, ill, iur);
  tmp.xpm("a.xpm", 12, gg);
  for (ill.j = 0; ill.j < tmp.ypoints(); ill.j++) {
  for (ill.i = 0; ill.i < tmp.xpoints(); ill.i++) {
    if ((int) tmp[ill] == 157) { 
      tmp[ill] = 0;
    }
    else {
      tmp[ill] = 1;
    }
  }
  }
  printf("area is %f\n",(float)tmp.integrate() / 1.e12) ;
  

  return 0;
}
