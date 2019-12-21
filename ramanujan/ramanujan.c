#include <stdio.h>
#include <math.h>

int main(void ) {
  long long int x, x3, y, y3, z, z3;
  float rat, trial, lower;

  rat = pow(1./2., 1./3.);

  for (z = 0*1000+1; z < 2000*1000; z++) {
    //printf("z = %d\n",(int) z); fflush(stdout);

    trial = rat*z;
    z3 = z*z*z+(long long int) 1;

    for (x = (long long int) floor(trial); x < z; x++) {
      x3 = x*x*x;
      y3 = z3 - x3;
      lower = floor( pow((long double) y3, (long double) 1./3.) );
 
      for (y = lower ; y < lower+2; y++) {
        y3 = y*y*y;
        if ( (z3 - x3 - y3) == 0) {
          printf("valid %7lld %7lld %7lld  %22lld\n",x,y,z,z3);
          fflush(stdout);
          break;
        }
      }
    }

  }

  //  printf("%d %d %d\n",sizeof(int), sizeof(long int), sizeof(long long int) );
  return 0;
}
