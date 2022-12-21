#include "ncepgrids.h"

#include "geometry.C"
int bcount = 0;

int main(void) {
  global_12th<unsigned char> x;
  global_12th<float> y;
  ijpt loc;
  mvector<ijpt> tri(4);
  FILE *fin;
  double sum = 0, sumsq = 0;
  int count = 0;

  fin = fopen("global_5min","r");
  x.binin(fin);
  fclose(fin);

  tri[0].i = 0;             tri[0].j = 0;
  tri[1].i = x.xpoints()/2; tri[1].j = x.ypoints()/2;
  tri[2].i = x.xpoints()-1; tri[2].j = 0;
  tri[3] = tri[0]; // needed by winding number

  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    y[loc] = (float) x[loc];
    if (wn_PnPoly(loc,tri,4) != 0) {
	    sumsq += y[loc]*y[loc];
	    sum   += y[loc];
	    count += 1;
    }
  }
  }
  printf("rms = %f   %f %f %f  %d\n",y.rms(), (float) sum/(float)count, 
	(float) sumsq/(float)count, 
	(float) ((sumsq - sum*sum/(double)count)/count), count );
    

  return 0;
}
