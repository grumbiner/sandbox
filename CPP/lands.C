#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  global_quarter<short int> qdoi;
  global_12th<unsigned char> rtghr;
  ijpt loc1, loc2;
  latpt ll;
  FILE *fin1, *fin2;
  int rtgland = 157, qdoiland = 0;
  int rtgwater = 0, qdoiwater = 1;

  fin1 = fopen(argv[1],"r");
  qdoi.binin(fin1);
  fclose(fin1);
  fin2 = fopen(argv[2],"r");
  rtghr.binin(fin2);
  fclose(fin2);

  for (loc1.j = 0; loc1.j < rtghr.ypoints(); loc1.j++) {
  for (loc1.i = 0; loc1.i < rtghr.xpoints(); loc1.i++) {
    ll = rtghr.locate(loc1);
    loc2 = qdoi.locate(ll);
    if ( !( (rtghr[loc1] == rtgland)  && (qdoi[loc2] == qdoiland) ) && 
         !( (rtghr[loc1] == rtgwater) && (qdoi[loc2] == qdoiwater) ) ) {
      printf("%4d %4d  %4d %4d   %3d %3d\n",loc1.i, loc1.j, loc2.i, loc2.j, 
                     rtghr[loc1], qdoi[loc2] );
    }
  }
  }

  return 0;
}
