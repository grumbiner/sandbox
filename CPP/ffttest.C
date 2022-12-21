#include <stdio.h>
#include <math.h>

#include "mvector.h"
#include "time_series.h"

//Test the fft member of the time_series class.
//Robert Grumbine

// 4 October 1999

#define NX 365

int main(void) {
   time_series<float> g(NX), f(NX), im(NX), re(NX);
   int i;

   for (i = 0; i < NX; i++) {
     f[i] = sin(2.*M_PI*i/16.);
   }
   f.fft(re, im);
//   g.ifft(re, im);
   g = f;

   for (i = 0; i < NX; i++) {
     printf("%3d  %f %f %f\n",i, f[i], f[i] - g[i], 
                  re[i]*re[i]+ im[i]*im[i] );
   }
  
   return 0;
}
