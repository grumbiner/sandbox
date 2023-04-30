//30 July 1999
// Dot products, with and without mask values
#include "mvector.h"

template <class T>
float dot(mvector<T> &x, mvector<T> &y) ;
template <class T>
float dot(mvector<T> &x, mvector<T> &y, T landval) ;

template <class T>
float dot(mvector<T> &x, mvector<T> &y) {
  int i;
  float sum = 0.0;
  if (x.xpoints() != y.xpoints() ) {
    printf("cannot dot unequal sized vectors\n");
    return -1.;
  }
  for (i = 0; i < x.xpoints() ; i++) {
    sum += x[i]*y[i];
  }
  return sum;
} 
template <class T>
float dot(mvector<T> &x, mvector<T> &y, T landval) {
  int i;
  float sum = 0.0;
  if (x.xpoints() != y.xpoints() ) {
    printf("cannot dot unequal sized vectors\n");
    return -1.;
  }
  for (i = 0; i < x.xpoints() ; i++) {
    if (x[i] != landval && y[i] != landval) {
      sum += x[i]*y[i];
    }
  }
  return sum;
} 
