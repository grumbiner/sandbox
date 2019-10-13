#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  northgrid<float> x, y, lapd;
  FILE *fin;
  int icount = 0, lrgdel = 0;
  ijpt loc;

  fin = fopen(argv[1], "r");
  if ( fin == (FILE *) NULL) {
    printf("Failed to open file %s\n",argv[1]);
    return 1;
  }
  x.binin(fin);
  fclose(fin);
  
  fin = fopen(argv[2], "r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open file %s\n", argv[2]);
    return 2;
  }
  y.binin(fin);
  fclose(fin);

  x.laplace(lapd);

  for (loc.j = 0; loc.j < lapd.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < lapd.xpoints() ; loc.i++) {
    if (fabs(lapd[loc]) < 1./100.) {
      icount += 1;
    }
  }
  }
  printf("%d of %d points are ignorable\n", icount, lapd.xpoints() *lapd.ypoints() );
  printf("max, min lapl %f %f\n",lapd.gridmax(), lapd.gridmin() );

  icount = 0;
  x -= y;
  for (loc.j = 0; loc.j < lapd.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < lapd.xpoints() ; loc.i++) {
    if (fabs(x[loc]) < 1./100.) {
       icount += 1;
    }
    if (fabs(x[loc]) > 1.28) {
       lrgdel += 1;
    }
  }
  }
  printf("%d of %d points are slowly varying %d fast\n", icount, lapd.xpoints() *lapd.ypoints(), lrgdel );
  printf("max, min delta %f %f\n",x.gridmax(), x.gridmin() );

  return 0;
}
  
