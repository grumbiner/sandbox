//30 July 1999
#include "metric.h"

void findland(metricgrid<float> &land, metricgrid<float> &conc, float landval) ;

void findland(metricgrid<float> &land, metricgrid<float> &conc, float landval) {
  int i;
  for (i = 0; i < land.xpoints()*land.ypoints(); i++) {
    if (conc[i] == landval) {
      land[i] = landval;
    }
    else {
      land[i] = 0.0;
    }
  }
}

