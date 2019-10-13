#include <stdio.h>
#include <time.h>
#include "grid_base.h"
#include "points.h"

class allpt : public ijpt {
  public:
    allpt();
    allpt& next(int nx) { if (i < nx-1) { i++; }
       else { i = 0; j += 1; }
       return *this;
     }
    allpt& first();
    bool operator>(allpt &);
};
allpt::allpt() {
  i = 0;
  j = 0;
}
bool allpt::operator>(allpt &x) {
  if (j > x.j) {return true;}
  else { return false; }
}

allpt& allpt::first() {
  i = 0;
  j = 0;
  return *this;
}


class fred : public grid2_base<float> {
  public:
    allpt curloc;
    fred();
    void forall(float (*fn)(float& ) ); // forall applies fn to all elements of fred
    allpt & next() { return curloc.next(nx); }
    allpt & first() ;
};
allpt & fred::first() {
  curloc.i = 0;
  curloc.j = 0;
  return curloc;
}
fred::fred() {
  nx = 1024*1;
  ny = 1024*1;
  grid = new float[nx*ny];
  curloc.i = 0;
  curloc.j = 0;
}
void fred::forall(float (*fn) (float &  ) ) {
  allpt loc;
  for (loc = first() ; in(loc)  ; loc = next()) {
    this->operator[](loc) = fn( this->operator[](loc) );
  }
}

float f1(float  &x) {
  return 5;
}
float f2(float  &x) {
  return 20;
}

int main(void) { 
  fred a;
  int i;
  allpt tloc;

  i = 0;
  printf("before a.next version %d \n",(int)clock() );
  for (tloc = a.first(); a.in(tloc) ; tloc = a.next()) {
    i += 1;
  }
  printf("before tloc.next version %d \n",(int)clock() );
  for (tloc = a.first(); a.in(tloc) ; tloc.next(a.xpoints()) ) {
    i += 1;
  }
  printf("after tloc.next %d \n",(int)clock() );
  printf(" i = %d\n",i);

  printf("before forall f1 %d \n",(int)clock() );
  a.forall(&f1);
  printf("after forall f1 %d \n",(int)clock() );
  printf("f1 %f\n", a[5]);

//  a.forall(&f2);
//  printf("after forall f2 %d \n",(int)clock() );
//  printf("f2 %f\n", a[5]);

  return 0;
}
