#include "grib.h"
#include "ncepgrids.h"

class beta : public grib_pds, grib_gds, grid2<float> {
  public:
    beta();
};
beta::beta() {
  grid2<float>::resize(30, 30);
}

int main(void) {
  beta x;

  x.set_precision(0.001);

  return 0;
}
