#include "ncepgrids.h"

//typedef struct {
//  mrf1deg<float> land, icec, t2m, u10, v10;
//} inputs;


int main(int argc, char *argv[]) {
  FILE *fin1, *fin2, *fout;
  mrf1deg<float> v1[5], v2[5], delta[5];
  mrf1deg<bool> differ;

  float flag = 0.0;
  double area = 0.0, sum1 = 0.0, sum2 = 0.0, sum = 0.0;
  ijpt loc;
  latpt ll;
  int i;
  float toler = 0.3, extreme = 1.6; // icing, inches per hour
  float t2_toler = 2.5, w_toler = 2.5;

  fin1 = fopen(argv[1], "r");
  fin2 = fopen(argv[2], "r");
  fout = fopen(argv[3], "w");

  differ.set(false);

  for (i = 0 ;i < 5; i++) {
    sum = 0.;
    sum1 = 0.;
    sum2 = 0.;
    area = 0.;
    v1[i].binin(fin1);
    v2[i].binin(fin2);
    delta[i] = v1[i];
    delta[i] -= v2[i];
    delta[i].binout(fout);

    for (loc.j = 0; loc.j < delta[i].ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < delta[i].xpoints(); loc.i++) {
      if (i == 0 && ( v1[i][loc] != v2[i][loc] ) ) {
        differ[loc] = true;
      }
      if (i == 1 && ( v1[i][loc] != v2[i][loc] ) ) {
        differ[loc] = true;
      }
      if (i == 2 && ( fabs(delta[i][loc]) > t2_toler) ) {
        ll = delta[i].locate(loc);
        printf("%2d temp %6.1f %6.1f  %.2f %.2f %5.2f\n",i, ll.lat, ll.lon, v1[i][loc], v2[i][loc], delta[i][loc]);
        differ[loc] = true;
      }
      if ( (i == 3 || i == 4)  && (fabs(delta[i][loc]) > w_toler) ) {
        ll = delta[i].locate(loc);
        printf("%2d wind %6.1f %6.1f  %.2f %.2f %5.2f\n",i, ll.lat, ll.lon, v1[i][loc], v2[i][loc], delta[i][loc]);
        differ[loc] = true;
      }

      sum1 += v1[i][loc]*delta[i].cellarea(loc);
      sum2 += v2[i][loc]*delta[i].cellarea(loc);
      sum  += delta[i][loc]*delta[i].cellarea(loc);
//      if (v1[i][loc] != 0 || v2[i][loc] != 0) {
//        area += delta[i].cellarea(loc);
//      }

    }
    }
 

    printf("%2d max %f %f %f rms %f %f %f sum %e %e %e area %e \n", i,
      v1[i].gridmax(), v2[i].gridmax(), delta[i].gridmax(),
      v1[i].rms(flag), v2[i].rms(flag), delta[i].rms(flag),
      sum1, sum2, sum, area );

  }


  for (loc.j = 0; loc.j < differ.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < differ.xpoints(); loc.i++) {
    if (differ[loc]) {
      ll = differ.locate(loc);
      for (i = 0; i < 5; i++) {
        printf("%2d differ %6.1f %6.1f  %.1f %.1f %4.1f\n",i, ll.lat, ll.lon, v1[i][loc], v2[i][loc], delta[i][loc]);
      }
    }
  }
  }

  
  return 0;
}
