#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin1, *fin2, *fout;
  mrf1deg<float> v1, v2, delta;
  float flag = 0.0;
  palette<unsigned char> gg(19,65);
  double area = 0.0, sum1 = 0.0, sum2 = 0.0, sum = 0.0;
  ijpt loc;
  latpt ll;
  float toler = 0.3, extreme = 1.6; // inches per hour

  fin1 = fopen(argv[1], "r");
  fin2 = fopen(argv[2], "r");
  fout = fopen(argv[3], "w");

  for (int i = 0 ;i < 57; i++) {
    sum = 0.;
    sum1 = 0.;
    sum2 = 0.;
    area = 0.;
    v1.binin(fin1);
    v2.binin(fin2);
    v1 *= 3600.*39.37; // inches per hour
    v2 *= 3600.*39.37; // inches per hour
    delta = v1;
    delta -= v2;
    delta.binout(fout);

    for (loc.j = 0; loc.j < delta.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < delta.xpoints(); loc.i++) {
      if ( fabs(delta[loc]) > toler && (v1[loc] < extreme || v2[loc] < extreme)  ) {
        ll = delta.locate(loc);
        printf("%2d toler %f %f  %f %f %f\n",i, ll.lat, ll.lon, v1[loc], v2[loc], delta[loc]);
      }
      sum1 += v1[loc]*delta.cellarea(loc);
      sum2 += v2[loc]*delta.cellarea(loc);
      sum  += delta[loc]*delta.cellarea(loc);
      if (v1[loc] != 0 || v2[loc] != 0) {
        area += delta.cellarea(loc);
      }
    }
    }
    printf("%2d max %f %f %f rms %f %f %f sum %e %e %e area %e \n", i,
      v1.gridmax(), v2.gridmax(), delta.gridmax(),
      v1.rms(flag), v2.rms(flag), delta.rms(flag),
      sum1, sum2, sum, area );

  }
  
  return 0;
}
