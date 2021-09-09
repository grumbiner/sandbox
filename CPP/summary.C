#include "ncepgrids.h"
//Print out some summary statistics -- max, min, average, area, extent, 
//  and a look at what points can be ignored on grounds of being average of
//  neighbors
//Robert Grumbine 10 Aug 2006

int main(int argc, char *argv[]) {
  GRIDTYPE<DTYPE> ice, tmp;
  mvector<int> vals(256);
  double area, extent;
  ijpt loc;
  latpt ll;
  FILE *fin;
  int i;

  fin = fopen(argv[1],"r");
  ice.binin(fin);
  fclose(fin);

  printf("max, min, average %f %f %f\n",(float) ice.gridmax(), 
              (float) ice.gridmin(), (float) ice.average() );
  if (ice.gridmax() < 3.0) ice *= 100;
  tmp = ice;

  vals = 0;
  for (loc.j = 0; loc.j < tmp.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < tmp.xpoints(); loc.i++) {
    vals[(int) (tmp[loc] + 0.5)] += 1;
  }
  }
  printf("frequency distribution \n");
  for (i = 0; i < vals.xpoints(); i++) {
    if (vals[i] != 0) printf("%3d occurs %d times\n",i, vals[i]);
  }
     
  for (loc.j = 0; loc.j < tmp.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < tmp.xpoints(); loc.i++) {
    if (tmp[loc] > 128) tmp[loc] = 0;
  }
  }
  area = tmp.integrate() / 100.;
  
  for (loc.j = 0; loc.j < tmp.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < tmp.xpoints(); loc.i++) {
    if (tmp[loc] > 0) tmp[loc] = 1;
  }
  }
  extent = tmp.integrate() ;
  
  printf("area = %f, extent = %f millions km^2 \n",area / 1.e12, extent / 1.e12);

// Laplace replacement test:
  GRIDTYPE<DTYPE> lapd;
  int icount = 0, acount = 0;
  ice.laplace(lapd);

  for (loc.j = 0; loc.j < lapd.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < lapd.xpoints() ; loc.i++) {
    if (ice[loc] != 0 && ice[loc] <= 100 ) { 
      acount++; 
      if (fabs((float) lapd[loc]) < 1. ) {
        icount += 1;
      }
    }
  }
  }
  printf("%d of %d points, %f pct are ignorable\n", icount, acount,
    (float) icount / (float) (acount) );
  printf("max, min lapl %f %f\n",lapd.gridmax(), lapd.gridmin() );


  return 0;
}
