#include <stdio.h>
#include <math.h>

float iexp(float x, float p);
float rexp(float x, float p);

main()
{
  float (*point)(float x, float p);
  float x, rp;
  int p;

  x = 273.15;
  rp = 7./2.;
  point = &rexp;
  printf("x ** rp is %f \n",point(x, rp));
  p = 4;
  point = &iexp;
  printf("x ** p is %f \n", point(x, (float)p));

}
float iexp(float x, float p)
{
  float tempor;
  int power;
  power = (int) p;
  tempor = log(x);
  tempor *= power;
  return (exp(tempor));
}
float rexp(float x, float p)
{
  float tempor;
  tempor = log(x);
  tempor *= p;
  return (exp(tempor));
}
