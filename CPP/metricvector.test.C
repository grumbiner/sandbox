#include <stdio.h>

#include "mvector.h"

//Program to exercise all of the elements of the mvector and
// metricvector classes
//Test on each and every modification to mvector.h 
//Robert Grumbine 25 January 1999

#define VEC_LENGTH (2*1024*1024)

int main(void) {
  int i;
//Testing construction of metricvector -- with and without arguments
  metricvector<float> a(VEC_LENGTH), b(VEC_LENGTH), z, *y;
  mvector<float> c(VEC_LENGTH);
  mvector<float> d(VEC_LENGTH);

  y = new metricvector<float> (VEC_LENGTH);
  printf("Test of constructions, following should be %d, 1, %d, is %d, %d, %d\n",
       VEC_LENGTH, VEC_LENGTH, a.xpoints(), z.xpoints(), y->xpoints() ); fflush(stdout);
  printf("assigning a constant to a metricvector in order to have some delay\n");
  a = (float) 1.0;
  b = (float) 2.0;
  c = (float) 3.0;
  delete y;
  printf("Just attempted to delete a newed y, free space should have jumped\n");
  fflush(stdout);
// End of construction/destruction testing

// Test of xpoints operator, assignment operator, [] operator, and == operator
  printf("xpoints test: %d should be defined value of %d\n", 
             a.xpoints(), VEC_LENGTH );  
  printf("simple assignment test: following should be 1 and 2, respectively %f %f\n",a[5], b[5]);
  printf("First test of equality operator between metricvector, this one should say false\n");
  if ( a == b) { 
    printf("a is == b\n");
  }
  else {
    printf("a is not = b\n");
  }
  a[5] = 72.;
  b[5] = 23.;
  printf("Completing the [] test, the following should be 72 and 23, respectively %f %f\n",a[5], b[5]);

  a = b;
  printf("metricvector assignment test, following should both be 23 %f %f ", a[5], b[5]);
  printf("while the following should be 2 %f %f\n",a[1], b[1]);
  printf("Complete test of equality operator between metricvector, this one should say true\n");
  if ( a == b) { 
    printf("a is == b\n");
  }
  else {
    printf("a is not = b\n");
    for (i = 0; i < a.xpoints(); i++) {
      if (a[i] != b[i] ) {
        printf("failed equality at %d %f %f\n",i, a[i], b[i]);
      }
    }
  }

  a = c;
  printf("should be 3 from mvector.assign %f\n",a[3]);
  for (i = 0; i < VEC_LENGTH; i++) {
     a[i] = 2*i*i;
     c[i] = 2*i*i;
     d[i] =   i;
  }
  a.set_metric(c);
  b.set_metric(d);
  b = 0.;
  printf("The following should have value of zero, we're before the interpolation\n");
  for (i = 0; i < 100; i++) {
    printf("i %3d val %f\n",i, b[i]);
  }

  b.interp(a, -1.0);
  printf("The following should have val about equal to i\n");
  for (i = 0; i < 100; i++) {
    printf("i %3d val %f \n",i, b[i]);
  }

  a.get_metric(d);
  a.get_metric(c);
  printf("the following should be 2*i*i\n");
  for (i = 0; i < 100; i++) {
    printf("i*i %d val %f \n",2*i*i, d[i]);
  }
  d[10] = d[9] + 1; //slightly different d
  b.set_metric(d);
  b.interp(a, -1.0);
  for (i = 0; i < 20; i++) {
    printf("i*i %d vals %f %f depths %f %f\n",2*i*i, b[i], a[i], d[i], c[i]);
  }
  


  printf("About to return\n"); fflush(stdout);

  return 0;
}
