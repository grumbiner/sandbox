#include "ncepgrids.h"
template <class T>
class icefamily : public northgrid<T> {
  public:
    icefamily(float = 1.0);
};
template <class T>
icefamily<T>::icefamily(float n) {
  nx = (int) (0.5 + nx*n);
  ny = (int) (0.5 + ny*n);
  dx /= n;
  dy /= n;
  delete []grid;
  grid = new T[nx*ny];
  pds.set_gridid(255);
  return;
}

