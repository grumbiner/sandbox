#include "ncepgrids.h"

int main(int argc, char *argv[] ) {
  FILE *fin;
  global_12th<float> sst, squared;
  float flag = -999.0;
  palette<unsigned char> gg(19, 65);
  ijpt loc;

  fin = fopen(argv[1], "r");
  sst.binin(fin);
  fclose(fin);
  gradsq(sst, squared, flag);
  printf("gradsq max, min, average, rms %e %e %e %e\n",
    squared.gridmax(), squared.gridmin(), squared.average(), squared.rms() );

  squared /= squared.rms();
  squared += 1;
  for (loc.j = 0; loc.j < sst.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < sst.xpoints(); loc.i++) {
    squared[loc] = log(min(100.0, squared[loc]) );
  }
  }

  squared.scale();
  squared.xpm("gradsq.xpm",7,gg);

  return 0;

}
