#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  global_12th<float> sst;
  int ti, tj;
  ijpt loc;
  latpt ll;

  ti = atoi(argv[1]);
  tj = atoi(argv[2]);
  loc.i = ti; loc.j = tj;
  ll = sst.locate(loc);
  printf("location %d %d argc = %d\n", loc.i, loc.j, argc);
  printf("latlon = %f %f\n",ll.lat, ll.lon);

  for (int i = 3; i < argc; i++) {
    fin = fopen(argv[i],"r");
    #ifdef RTG
    sst.binin(fin);
    #else
    sst.ftnin(fin);
    #endif
    fclose(fin);
    printf("%3d %5.2f\n",i-2, sst[loc]); 
    fflush(stdout);
  }

  return 0;
}
