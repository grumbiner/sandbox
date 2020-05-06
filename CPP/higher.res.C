#include <stdio.h>
#include "ncepgrids.h"

class highres : public northgrid<float> {
  public:
     highres();
};
highres::highres() {
  nx *= 2;
  ny *= 2;
  dx /= 2;
  dy /= 2;
  delete []grid; // This is necessary because instantiations proceed from
                 // the bottom up, so that grid is already 'new' (from 
                 // northgrid) but of the wrong size. 
  grid = new float[nx*ny];
  printf("highres grid = %d\n",grid); fflush(stdout);
}
class higher : public highres {
  public :
    higher();
};
higher::higher() {
  nx *= 2;
  ny *= 2;
  dx /= 2;
  dy /= 2;
  delete []grid; // This is necessary 
  grid = new float[nx*ny];
  printf("higher grid = %d\n",grid); fflush(stdout);
}

int main(void) {
  higher beta;

  beta.set(2.5);
  printf("beta nx = %d\n",beta.xpoints() ) ;
  return 0;
}

