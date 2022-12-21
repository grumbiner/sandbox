#include <stdio.h>

#include "points.h"

// Program to test the points class(es)
// Robert Grumbine
// Last Modified 4 May 1998

int main(void) {
  point3<float> x0, x1(1), x2(1,2), x3(1,2,3);
  point3< point3<float> > y(x1, x2, x3);
  fijpt f, g;
  ijpt ijf, ijg;
  float m;
  int i;
  float fl;
  double db;

// Tests of elementary point3's.
  printf("Test of constructor\n");
  printf("%f %f %f\n",x0.i, x0.j, x0.k);
  printf("%f %f %f\n",x1.i, x1.j, x1.k);
  printf("%f %f %f\n",x2.i, x2.j, x2.k);
  printf("%f %f %f\n",x3.i, x3.j, x3.k);
  printf("y point3 <point3<float> >\n");
  m = 2.;
  printf("%f %f %f \n",y.i.i, y.i.j, y.i.k);
  printf("%f %f %f \n",y.j.i, y.j.j, y.j.k);
  printf("%f %f %f \n",y.k.i, y.k.j, y.k.k);

  printf("Test of utility operators\n");
  m = x3.magnitude();
  printf("x3 magnitude = %f\n",m);
  x3.normalize();
  m = x3.magnitude();
  printf("After normalization\n");
  printf("%f %f %f\n",x3.i, x3.j, x3.k);
  printf("x3 magnitude = %f\n",m);

  printf("Test of arithmetic on point3<float> with scalars\n");
  m = 2.0;
  x3 += m;
  printf("add   %f %f %f\n",x3.i, x3.j, x3.k);
  x3 -= m;
  printf("minus %f %f %f\n",x3.i, x3.j, x3.k);
  x3 *= m;
  printf("times %f %f %f\n",x3.i, x3.j, x3.k);
  x3 /= m;
  printf("div   %f %f %f\n",x3.i, x3.j, x3.k);
  x3 = m;
  printf("equal %f %f %f\n",x3.i, x3.j, x3.k);

  printf("Test of arithmetic between point3s<floats>\n");
  x3 = x2;
  printf("%f %f %f\n",x2.i, x2.j, x2.k);
  x2 += x3;
  printf("%f %f %f\n",x2.i, x2.j, x2.k);
  x2 -= x3;
  printf("%f %f %f\n",x2.i, x2.j, x2.k);
  x2 += x3;
  printf("%f %f %f\n",x2.i, x2.j, x2.k);
  x2 -= x3;
  printf("%f %f %f\n",x2.i, x2.j, x2.k);
  x2 *= x3;
  printf("%f %f %f\n",x2.i, x2.j, x2.k);

  printf("Logical operations\n");
  printf("x2 %f %f %f\n",x2.i, x2.j, x2.k);
  printf("x3 %f %f %f\n",x3.i, x3.j, x3.k);
  if (x2 == x3) {
    printf("x2 == x3\n");
  }
  else {
    printf("x2 != x3\n");
  }

  if (x2 != x3) {
    printf("x2 != x3\n");
  }
  else {
    printf("x2 == x3\n");
  }

  if (x2 >= x3) {
     printf("x2 >= x3\n");
  }
  else {
     printf("x2 not >= x3\n");
  }

  if (x2 <= x3) {
     printf("x2 <= x3\n");
  }
  else {
    printf("x2 not <= x3\n");
  }

  if (x2 > x3) {
     printf("x2 > x3\n");
  }
  else {
     printf("x2 not > x3\n");
  }

  if (x2 < x3) {
     printf("x2 < x3\n");
  }
  else {
    printf("x2 not < x3\n");
  }

  printf("\n Test of casting\n");
  i = (int) x3;
  fl = (float) x3;
  db = (double) x3;
  printf("cast %d %f %f\n",i, fl, db);

// Test of point3<point3<float>>
  { point3<point3<float> > mpoint;
    point3<float> tpoint;
    printf("y point3 <point3<float> >\n");
    tpoint = 2.;
    mpoint = tpoint;
    printf("%f %f %f \n",y.i.i, y.i.j, y.i.k);
    printf("%f %f %f \n",y.j.i, y.j.j, y.j.k);
    printf("%f %f %f \n",y.k.i, y.k.j, y.k.k);
    printf("About to multiply y by mpoint\n");
    y *= mpoint;
    //printf("y * mpoint %f\n", mpoint);
    printf("%f %f %f \n",y.i.i, y.i.j, y.i.k);
    printf("%f %f %f \n",y.j.i, y.j.j, y.j.k);
    printf("%f %f %f \n",y.k.i, y.k.j, y.k.k);
    y /= mpoint;
    printf("y / 2\n");
    printf("%f %f %f \n",y.i.i, y.i.j, y.i.k);
    printf("%f %f %f \n",y.j.i, y.j.j, y.j.k);
    printf("%f %f %f \n",y.k.i, y.k.j, y.k.k);
    y += mpoint;
    printf("y + 2\n");
    printf("%f %f %f \n",y.i.i, y.i.j, y.i.k);
    printf("%f %f %f \n",y.j.i, y.j.j, y.j.k);
    printf("%f %f %f \n",y.k.i, y.k.j, y.k.k);
    y -= mpoint;
    printf("y - 2\n");
    printf("%f %f %f \n",y.i.i, y.i.j, y.i.k);
    printf("%f %f %f \n",y.j.i, y.j.j, y.j.k);
    printf("%f %f %f \n",y.k.i, y.k.j, y.k.k);
  }
  m = 2.;
  f = y.k;
  printf("fijpt f %f %f %f\n", f.i, f.j, f.k);
  g = f;
  printf("fijpt g %f %f %f\n", g.i, g.j, g.k);
  f += g;
  printf("fijpt f %f %f %f\n", f.i, f.j, f.k);
  f *= m;
  printf("fijpt f %f %f %f\n", f.i, f.j, f.k);
  f /= m;
  printf("fijpt f %f %f %f\n", f.i, f.j, f.k);
  f += m;
  printf("fijpt f %f %f %f\n", f.i, f.j, f.k);
  f -= m;
  printf("fijpt f %f %f %f\n", f.i, f.j, f.k);
  
  ijf = f;
  printf("ijpt %d %d %d\n", ijf.i, ijf.j, ijf.k);
  ijg = ijf;
  printf("ijpt %d %d %d\n", ijg.i, ijg.j, ijg.k);
  ijf += ijg ;
  printf("ijpt %d %d %d\n", ijf.i, ijf.j, ijf.k);
  ijf *= 2;
  printf("ijpt %d %d %d\n", ijf.i, ijf.j, ijf.k);
  ijf /= 2;
  printf("ijpt %d %d %d\n", ijf.i, ijf.j, ijf.k);
  ijf += 2;
  printf("ijpt %d %d %d\n", ijf.i, ijf.j, ijf.k);
  ijf -= 2;
  printf("ijpt %d %d %d\n", ijf.i, ijf.j, ijf.k);

  



  return 0;
}
