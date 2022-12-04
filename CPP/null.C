#include "ncepgrids.h"

// construct null forecasters for sea ice cover, giving a baseline for
//   assessing skill of 'real' models
// Robert Grumbine 12 June 2014
//
// level 0 -- forecasters which have no knowledge of anything
//   a) ice everywhere
//   b) ice nowhere
//
// level 1A -- forecasters which know only history and climatology
//   a) ice same time last year (input)
//   b) climatological field (>= 50% of years) (conditional climatology input)
//
// level 1B -- forecaster which knows only yesterday
//   a) persistence (input)
//
// level 2 -- forecaster which knows yesterday, history, climatology
//   a) analog wrt history (full history !?)
//   b) sweep ice cover for N (# years of ice in grid points) and apply
//       same N to each area + adjacent points

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  global_ice<float> xlow;
  global_12th<float> xhigh;
  global_ice<float> climo;
  global_ice<int> nyears;

  return 0;
}
