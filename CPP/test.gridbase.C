#include <iostream>
#include "grid_base.h"

using namespace std;

// Demonstration and exercising of the base grid2 class, grid_base
// This can also be used to diagnose system efficiency

// Robert Grumbine
// 19 February 1999

int main(void) {
  grid2_base<float> x, y(20), z(40, 50);
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
  else {
    cout << "Y and X are not identical (should not be the case) \n";
  }

   
  
  




  cout << "\n\n\n";
  return 0;
}
