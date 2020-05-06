#include <stdio.h>
#include <time.h>

#include "mvector.h"

//Program to exercise all of the elements of the mvector class
//Test on each and every modification to mvector.h 
//Robert Grumbine 25 January 1999

#define VEC_LENGTH (6*1024*1024)

int main(void) {
  int i;
  float tmp_float;
//Testing construction of mvector -- with and without arguments
  mvector<float> a(VEC_LENGTH), b(VEC_LENGTH), z, *y;

//Testing construction of mvector -- with and without arguments
  y = new mvector<float> (VEC_LENGTH);
  printf("Test of constructions, following should be VEC, 1, VEC, is %d, %d, %d\n",
       a.xpoints(), z.xpoints(), y->xpoints() ); fflush(stdout);
  printf("assigning a constant to a mvector in order to have some delay\n");
  a = (float) 1.0;
  b = (float) 2.0;
  delete y;
  printf("Just attempted to delete a newed y, free space should have jumped\n");
  fflush(stdout);
// End of construction/destruction testing

// Test of xpoints operator, assignment operator, [] operator, and == operator
  printf("xpoints test: %d should be defined value of %d\n", 
             a.xpoints(), VEC_LENGTH );  
  printf("simple assignment test: following should be 1 and 2, respectively %f %f\n",a[5], b[5]);
  printf("First test of equality operator between mvector, this one should say false\n");
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
  a = a;
  printf("mvector to mvector assignment test, following should both be 23 %f %f ", a[5], b[5]);
  printf("while the following should be 2 %f %f\n",a[1], b[1]);
  printf("Complete test of equality operator between mvector, this one should say true\n");
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

////////////IO testing -- very large and slow for the large mvector used
//  f1 = fopen("vec.io.test", "a+");
//  rewind(f1);
//  a.binout(f1);
//  rewind(f1);
//  b = 235.;
//  b.binin(f1);
//  a -= b;  //Incidental -= test
//  a.printer(stdout);

////////Test of arithmetic operators
    a = 5.;
    b = 2.;
    printf("Time start %d\n",(int) clock() );
    a *= b; printf("*= vec should be 10 %f\n",a[5]);
    a /= b; printf("/= vec should be  5 %f\n",a[5]);
    a += b; printf("+= vec should be  7 %f\n",a[5]);
    a -= b; printf("-= vec should be  5 %f\n",a[5]);
    printf("Time end/start %d\n",(int) clock() );
    a *= 2.; printf("*= T should be 10 %f\n",a[5]);
    a /= 2.; printf("/= T should be  5 %f\n",a[5]);
    a += 2.; printf("+= T should be  7 %f\n",a[5]);
    a -= 2.; printf("-= T should be  5 %f\n",a[5]);
    printf("Time end %d\n",(int) clock() );

// Test of mvector functions which return a variable (no internal mods)
    printf("a norm, l2 %f\n",a.norm() );
    printf("a rms      %f\n",a.rms() );
    a[0] = 23.;
    printf("a.max 23   %f\n",a.maximum() );
    a[4] = -70.;
    printf("a.min -70  %f\n",a.minimum() );
    a[0] = a[1];
    a[4] = a[1];
    printf("a.average  5 %f\n",a.average() );
    for (int kk = 0; kk < 1024; kk++) {
      a[kk] = 23.;
    }
    printf("a.average  5 %f\n",a.average(23. ) );
    printf("a.complete   %f\n",a.complete(23.) ); 

// Test of mvector functions which modify a mvector
   for (i = 0; i < a.xpoints(); i++) {
      a[i] = (float) i;
   }
   a[7] = -1.0;
   tmp_float = -1.0;
   a.fill(tmp_float);
   printf("Following should be 6, 7, 8: %f %f %f\n",a[6], a[7], a[8]);

   tmp_float = 4.0;
   printf("Time before rescale %d\n",(int) clock() );
   a = a.rescale(tmp_float);
   printf("Time after rescale  %d\n",(int) clock() );
   printf("Following should be 1, 2, 3: %f %f %f\n",a[4], a[8], a[12]);
  
   printf("Time before normalize %d\n",(int) clock() );
   a.normalize();
   printf("Time after normalize  %d\n",(int) clock() ); fflush(stdout);
   printf("Norm of a should now be 1: %f\n",a.norm() );
    

  printf("About to return\n"); fflush(stdout);

  return 0;
}
