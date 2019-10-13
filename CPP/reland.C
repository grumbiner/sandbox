#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *f1, *f2;
  ijpt loc, gloc;
  latpt ll, gll;
  gaussian<float> gfs;
  global_12th<unsigned char> x;
  float dlon = 360./1152.;

  f1 = fopen(argv[1],"r");
  f2 = fopen(argv[2], "r");
  gfs.binin(f1);
  fclose(f1);
  x.binin(f2);
  fclose(f2);

  printf("dlon = %f\n",dlon);
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    ll = x.locate(loc);
    gll = ll;
    gll.lat = -gll.lat; 
    gloc = gfs.locate(gll);
    //gloc.i += 1; if (gloc.i == gfs.xpoints()) gloc.i = 0;
    if (x[loc] == 2 && gfs[gloc] == 0) {
      printf("hr land gfs water %7.3f %7.3f\n",ll.lat, ll.lon);
    }
    else if (x[loc] != 2 && gfs[gloc] == 1) {
      printf("hr water gfs land %7.3f %7.3f\n",ll.lat, ll.lon);
    }
  }
  }
   
  return 0;
}
