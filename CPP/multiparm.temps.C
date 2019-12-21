#include <stdio.h>

//Demonstration/test of a class which is multiply templated.
// The first element in this one is to define a class, with the
// second being a required parameter for the construction of the
// class. 
//Robert Grumbine 18 April 2000

#include "ncepgrids.h"

//Class declaration:
template <class T, int N>
class vgrid : public mvector<T> {
  public:
    int nx;
    mvector<T> *x;
    vgrid();
};
template <class T, int N>
vgrid<T, N>::vgrid() {
  x = new mvector<T> [N];
  nx = N;
}

//Main program.
//This example says that we want a mvector of 35 'northgrid's
//This sort of thing can be useful for a model of many layers.

int main(void) {
  vgrid<northgrid<float>, 35> model;
  float tmp = 5.0;

  model.x[0] = (float) 5.0;
  model.x[1] = tmp;

  return 0;
}
