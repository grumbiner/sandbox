#include <stdio.h>

#include "grid_math.h"
// Program to test the grid_math class (and antecedant grid_base)
// Robert Grumbine
// Last Modified 15 April 1998

int main(void) {
  grid2<float> x(4,4), y(4,4), z(4,4);
  
  x = 23.0;
  y = 42.0;
  x += y;
  y = x.laplace();
  printf("x = \n"); x.printer(stdout);
  printf("laplacean of x = \n"); y.printer(stdout);
  z = x.gradsq();
  printf("gradsq(x) = \n"); z.printer(stdout);

// Exercise the X= operators
  x.printer(stdout);
  printf("Using y = 42\n");
  y = 42.0;
  x += y;
  x.printer(stdout);
  x -= y;
  x.printer(stdout);
  x *= y;
  x.printer(stdout);
  x /= y;
  x.printer(stdout);

//Exercise the operators with scalars:
  printf("Exercise the operators with scalars\n");
  x += (float) 2.0;
  printf("x+=2\n");
  x.printer(stdout);
  x -= (float) 2.0;
  printf("x-=2\n");
  x.printer(stdout);
  x *= 2.;
  printf("x*=2\n");
  x.printer(stdout);
  x /= 2.0;
  printf("x/=2\n");
  x.printer(stdout);

//Now work with simple binary operators
  printf("Now try y = x + (float 2); x = \n"); fflush(stdout);
  x.printer(stdout);
  z = x + (float) 2.0;
  z.printer(stdout);
  printf("Now try y = x - (float 2)\n"); fflush(stdout);
  z = x - (float) 2.0;
  z.printer(stdout);
  printf("Now try y = x * (float 2)\n"); fflush(stdout);
  z = x * (float) 2.0;
  z.printer(stdout);
  printf("Now try y = x / (float 2)\n"); fflush(stdout);
  z = x / (float) 2.0;
  z.printer(stdout);
  
  printf("About to try z = x + y, x, y = \n");
  x.printer(stdout);
  y.printer(stdout);

  z = x + y;
  z.printer(stdout);
  printf("About to try z = x - y\n");
  z = x - y ;
  z.printer(stdout);
  printf("About to try z = x * y\n");
  z = x * y ;
  z.printer(stdout);
  printf("About to try z = x / y\n");
  z = x / y ;
  z.printer(stdout);


  return 0;
}
