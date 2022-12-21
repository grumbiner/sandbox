#include <stdio.h>
#include <math.h>

//This is the start of framing a parameters class
#include "params.h"

int main(void) {
  parameters parm;
  
  printf("%f is g(45N)\n",       parm.g(45.*M_PI/180.0) );
  printf("%f is g(45N, 10km)\n", parm.g(45.*M_PI/180.0, 10000.) );
  printf("%f is cpair\n",        parm.cp_air);

  return 0;
}
