#include <stdio.h>

//Program to demo the use of multiple classes in a template
//Robert Grumbine
//Last Modified 29 January 1998

template <class T, class U>
class fred {
  public:
    T x;
    U y;
    fred();
    float add();
};
template <class T, class U>
fred<T,U>::fred() {
  x = (T) 5;
  y = (U) 3;
}
template <class T, class U>
float fred<T,U>::add() {
  float z;
  z = ((float)x) + ((float)y);
  return z;
}


template <class T, class U>
class george {
 public:
  T x;
  george();
  float add(U &);
};
template <class T, class U>
george<T,U>::george() {
  x = 5;
}
template <class T, class U>
float george<T,U>::add(U &y) {
  float z;
  z = (float) (x + (T) y) ;
  return z;
}



int main(void) {
  fred<float, int> a;
  george<float, int> c;
  float b;
  int num = 5;

  b = a.add();
  printf("b = %f\n",b);
  
  b = c.add(num);
  printf("b = %f\n",b);
  
  return 0;
}
