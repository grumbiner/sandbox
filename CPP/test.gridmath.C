#include <stdio.h>
#include <time.h>
#include <iostream>
using namespace std;

#include "grid_math.h"

//Test the grid_math class.  Oriented more to execution speed than
//  class function demonstration
//Robert Grumbine November 20  2000

void gbase_sub();
#define NY (1024)
#define NX (1024)

int main(void) {
  grid2<float> x, y(20), z(40, 50);
  float val1=5., val2=6.;
  ijpt i1;
  clock_t start, end;
  int nbit = 16;

//Test that we can still do all the usual grid_base things
  gbase_sub();
  cout << "\n\n\n";

//Begine math lib specific things
  x.resize(NX, NY);
  y.resize(NX, NY);
  z.resize(NX, NY);
  i1.i = 5; i1.j = 7;
  x = val1;
  z = val2;
  y = val2;

  x += z;
  cout << x[i1] << " Should be " << val1+val2 << "\n";
  x -= z;
  cout << x[i1] << " Should be " << val1 << "\n";
  x *= z;
  cout << x[i1] << " Should be " << val1*val2 << "\n";
  x /= z;
  cout << x[i1] << " Should be " << val1 << "\n";
  x += val2;
  cout << x[i1] << " Should be " << val1+val2 << "\n";
  x -= val2;
  cout << x[i1] << " Should be " << val1 << "\n";
  x *= val2;
  cout << x[i1] << " Should be " << val1*val2 << "\n";
  x /= val2;
  cout << x[i1] << " Should be " << val1 << "\n";

//Now explicitly binary operators
  x = x + y;
  cout << x[i1] << " Should be " << val1+val2 << "\n";
  x = x - y;
  cout << x[i1] << " Should be " << val1 << "\n";
  x = x * y;
  cout << x[i1] << " Should be " << val1*val2 << "\n";
  x = x / y;
  cout << x[i1] << " Should be " << val1 << "\n";

//Test timing
  start = clock();
  printf("Loop over x+= z %8.4f\n ", (float)clock()/ (float)CLOCKS_PER_SEC );
  for (int j = 0; j < 100; j++) {
    x += z;
  }
  end = clock();
  printf("                %8.4f ", (float)clock()/ (float)CLOCKS_PER_SEC );
  printf("Approx mflops = %f\n",(float)(100*NY*NX/1024/1024)/
          ((float) (end - start)/(float)CLOCKS_PER_SEC)       );

  //cout << "Loop over x = x + z " << clock(); 
  //for (int j = 0; j < 10; j++) {
  //  x = x + z;
  //}
  //cout << " " << clock() << "\n";

// Start testing grid level operations, for timing purposes repeat the loop:
for (int jjj = 0; jjj < 10; jjj++) {
  printf("Max in x is %f\n",x.gridmax() );
  printf("Min in x is %f\n",x.gridmin() );
  printf("average of x is %f\n",x.average());
  printf("average of x excluding val1's %f\n",x.average(val1) );
  z = x.laplace();
  printf("average of the laplacean of x %f\n",z.average() );
  x.laplace(z);
  printf("average of the laplacean of x %f\n",z.average() );
  z = x.gradsq();
  printf("average of the squared gradient of x %f\n",z.average() );
  x.gradsq(z);
  printf("average of the squared gradient of x %f\n",z.average() );
  printf("rms of x %f\n",x.rms() );
  printf("rescaling of x \n"); x.scale();
  printf("grib scaling of z\n"); z.grib_scale(3, nbit, x);

//Test the reduction operator:
  for (i1.j = 0; i1.j < x.ypoints() ; i1.j++) {
  for (i1.i = 0; i1.i < x.xpoints() ; i1.i++) {
    x[i1] = i1.i;
  }
  }
  z.resize(NX/2, NY/2);
  printf("reduce          %8.4f\n", (float)clock()/ (float)CLOCKS_PER_SEC );
  z.reduce(x);
  printf("                %8.4f\n", (float)clock()/ (float)CLOCKS_PER_SEC );
  //z.printer(stdout);
}

  return 0;
}

/////////////////////////////////////////////
//Grid2's as grid2_bases:
void gbase_sub() {

  grid2<float> x, y(20), z(40, 50);
  float tmpflt = 7.5;
  ijpt i1, i2;
  int i;

  cout << "Size of x " << x.xpoints() << " " << x.ypoints() << "\n";
  cout << "Size of y " << y.xpoints() << " " << y.ypoints() << "\n";
  cout << "Size of z " << z.xpoints() << " " << z.ypoints() << "\n";
  x.resize(20, 20);
  cout << "After trying to resize x to 20x20, destroying any extant data in the process\n";
  cout << "Size of x " << x.xpoints() << " " << x.ypoints() << "\n";

  y.resize(20, 20);
  x.set(5.0);
  y.set(7.0);
  i1.i = 5;
  i1.j = 5;
  cout << y[i1] << " " << x[i1] << "\n";
  y.set(x);
  cout << y[i1] << " " << x[i1] << "\n";
  y[i1] = 23.0;
  cout << y[i1] << " " << x[i1] << "\n";
  i = 22;
  cout << y[i] << "\n";


  if (y.in(i1) ) {
    cout << "In is in grid\n";
  }
  else {
    cout << "In is not in grid\n";
  }
  y[i1] = 23.0;
  i1.j = 6;
  y[i1] = 23.0;
  cout << y[i1] << "\n" ;
  if (y.anyof(23.0, 3, i1)) {
    cout << "One or more points within 3 of i1 is indeed 23\n";
  }
  else {
    cout << "No points within 3 of i1 is indeed 23\n";
  }
  cout << "y.anyof " << y.anyof(23.0, 3, i1) << "\n";
  cout << y[i1] << "\n" ;

// Operator tests:
  y = tmpflt;
  cout << y[i1] << " Should be " << tmpflt << "\n" ;
  y = 2.*tmpflt;
  cout << y[i1] << " Should be " << 2.*tmpflt << "\n" ;
  if (y == x ) {
    cout << "Y and X are identical (should not be the case)\n";
  }
  else {
    cout << "Y and X are not identical (should be the case) \n";
  }
  y = x;
  if (y == x ) {
    cout << "Y and X are identical (should be the case)\n";
  }
  cout << "Leaving the grid_base portion of grid_math\n\n\n";

}
