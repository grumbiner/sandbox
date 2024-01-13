// 30 July 1999
#include "mvector.h"

float independ(mvector<int> &x, mvector<int> &y) ;

//Compare two binary mvector for independance
float independ(mvector<int> &x, mvector<int> &y) {
  mvector<int> z(x.xpoints() );
  float px, py, pz;

  if (x.xpoints() == 0) return 0;
  z = y;
  z *= x;
  px = x.norm(1);
  py = y.norm(1);
  pz = z.norm(1);

  // If the mvector are independant, then p(xy) = p(x) * p(y);
  // where we're letting z = xy.  Note that the norm(1) simply sums the
  // elements (i.e., a counting for binary series), and z = x*y is
  // the same for binary series as z = x AND y;
  if (pz != 0.) {
    return (pz - px*py)*x.xpoints() / sqrt(x.xpoints() * pz*(1.-pz) );
  }
  else if (px != 0.) {
    return (pz - px*py)*x.xpoints() / sqrt(x.xpoints() * px*(1.-px) );
  }
  else if (py != 0.) {
    return (pz - px*py)*x.xpoints() / sqrt(x.xpoints() * py*(1.-py) );
  }
  else {
    return 0;
  }

  return 0;
}
