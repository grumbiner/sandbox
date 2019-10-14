#include <stdio.h>
#include <math.h>

float bernoulli(float theta, int y, int n) ;

int main(void) {
// let p(theta) (prior) = bernoulli(theta, 10, 20)
// p(y|theta) = bernoulli(theta, y, N) (y, N = obsd, theta vary)
  int y, N;
  float theta, dtheta = 1 / 128.;
  float rescale, rescale1;

  rescale = 0.0;
  rescale1 = 0.0;
  y = 3; N = 10;
  for (theta = 0; theta <= 1; theta += dtheta) {
    rescale += dtheta * bernoulli(theta, y, N)*bernoulli(theta, 10, 20);
    rescale1 += dtheta * bernoulli(theta, 10, 20);
  }
  for (theta = 0; theta <= 1; theta += dtheta) {
    printf("%f %f %f\n",theta, bernoulli(theta, 10, 20)/rescale1, 
      bernoulli(theta, y, N)*bernoulli(theta, 10, 20) / rescale);
  }

  return 0;
}
  

float bernoulli(float theta, int y, int n) {
  return pow(theta,y)*pow(1-theta, n-y);
}
int combinations(n,k) {
  return 0;
}
