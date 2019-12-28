// 30 July 1999
#include "mvector.h"

float partition(mvector<int> &ice, mvector<float> &soi) ;

float partition(mvector<int> &ice, mvector<float> &soi) {
  int i, oncount = 0, offcount = 0;
  float off = 0., on = 0.;

  for (i = 0; i < ice.xpoints() ; i++) {
     if (ice[i] == 0) {
       off += soi[i];
       offcount += 1;
     }
     else {
       on  += soi[i];
       oncount += 1;
     }
  }
  if (oncount*offcount != 0) {
    return on/oncount - off/offcount;
  }
  else if (oncount == 0) {
    return -off/offcount;
  }
  else {
    return on / oncount;
  }

}
