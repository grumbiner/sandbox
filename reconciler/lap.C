#include "ncepgrids.h"

int main(void) {
  FILE *fin;
  ijpt loc;
  global_12th<unsigned char> x;
  global_12th<float> xf, lf;
  mvector<int> vals(1000);
  int i, tot = 0;

  fin = fopen("fout_i","r");
  x.binin(fin);
  fclose(fin);
  vals = 0;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    switch( x[loc] ) {
    case 1:
	    xf[loc] = 1.0; break;
    case 5:
	    xf[loc] = 5.0; break;
    case 15:
	    xf[loc] = 21.0; break;
    case 17:
	    xf[loc] = 85.0; break;
    default:
	    printf("out of range %d\n",x[loc]);
    }
    vals[(int)xf[loc] ] += 1;
  }
  }
  for (i = 0; i < vals.xpoints(); i++) {
    if (vals[i] != 0) {
      tot += vals[i];
      printf("%3d %7d\n",i,vals[i]);
    }
  }

  xf.laplace(lf);
  printf("lf max, min %f %f\n",lf.gridmax(), lf.gridmin() );
  vals = 0;
  int tmp = (int) lf.gridmin();
  for (loc.j = 0; loc.j < xf.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < xf.xpoints(); loc.i++) {
     vals[(int) lf[loc] - tmp ] += 1;
  }
  }
  for (i = 0; i < vals.xpoints(); i++) {
    if (vals[i] != 0) {
      tot += vals[i];
      printf("%3d %7d\n",i,vals[i]);
    }
  }
  
  return 0;
}
