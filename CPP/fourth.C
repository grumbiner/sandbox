#include <stdio.h>
#include <math.h>

// compute a 4th power rectified sine wave
// i.e., half-rectified sine wave solar input, black body temperature variation equal
int main(void) {
  float t, x, tmp;
  for (t = 0; t < 1; t += 0.001) {
    x = sin(2.*M_PI*t);
    if (x < 0) x = 0;
    tmp = pow(x,0.25);
    printf("%f %f %f\n",t,x,tmp);
  }

  return 0;
} 
