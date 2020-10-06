#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  global_12th<float> sst;
  latpt ll;
  ijpt loc;

  ll.lat = atof(argv[1]);
  ll.lon = atof(argv[2]);
  loc = sst.locate(ll);

  for (int i = 3; i < argc; i++) {
    //printf("fname = %s\n",argv[i]);
    fin = fopen(argv[i],"r");
    sst.binin(fin);
    printf("file %3d loc %7.3f %8.3f value = %6.2f\n",i-2, ll.lat, ll.lon, sst[loc]);
    fclose(fin);
  }

  return 0;
}
