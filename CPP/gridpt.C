#include "grid_math.h"

//Illustrate building a grid2 of something other than a simple
//  type.  In this case we have a grid2 of point3's.
//Robert Grumbine 18 April 2000

#define NX 512
#define NY 512

int main(void) {
  grid2<point3<float> > x(NX, NY), y(NX, NY);
  point3<float> z(1.0, 2.0, 3.0);
  int i;

  x.set(z);  // Initialize the grids with the values of z
  y.set(z);

  // Iterate the operations so as to be able to assess execution
  // efficiency.
  // Note that every operation is also undone, so that at the end
  // x is identically what it started out to be.
  for (i = 0; i < 100; i++) {
    x += y;
    x -= y;
    x *= y;
    x /= y;
  }

  // Pick off a single member of the x grid for printing.  
  // Values should be 1, 2, 3; as they were at the start.
  z = x[5];
  printf("%f %f %f\n",z.i, z.j, z.k);

  return 0;
}
