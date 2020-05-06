//30 July 1999
#include "metric.h"

void condsub(metricgrid<float> &orig, metricgrid<float> &avg, float landval) ;

//Conditional subtraction -- skip points where either is masked and use
//                           mask as result
void condsub(metricgrid<float> &orig, metricgrid<float> &avg, float mask) {
  int i;
  for (i =0; i < avg.xpoints() * avg.ypoints(); i++) {
    if (avg[i] != mask && orig[i] != mask ) { 
      orig[i] -= avg[i]; 
    }
    else {
      orig[i] = mask;
    }
  }
  return;
}
