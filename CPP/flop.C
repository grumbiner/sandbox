#include "mvector.h"

// Functions to estimate 'floppiness' of time series of ice concentrations.
// Idea is that real ice tends to have a simple mode switch, to or from
//   ice covered, while weather flops back and forth pretty randomly.
// Robert Grumbine 
//   4 -r--r--r--    1 rmg3     ice          1905 Oct 24  2000 flop.C
//   4 -r--r--r--    1 rmg3     ice          2899 Dec 11  2000 greatlakes.C
//   4 -r--r--r--    1 rmg3     ice          2259 Oct  5  2001 northflop.C
//   4 -r--r--r--    1 rmg3     ice          2326 Oct 26  2001 nsidcflop.C


template <class T>
float trials(mvector<T> &series) {
  int i, nonzero = 0, flip = 0;

  nonzero = 0;
  flip = 0;
  for (i = 0; i < series.xpoints(); i++) {
    if (series[i] != 0) {
      nonzero += 1;
    }
    if (i != 0) {
      if ( ( (series[i] != 0) && (series[i-1] == 0) ) ||
           ( (series[i] == 0) && (series[i-1] != 0) )  ) {
      flip += 1;
    }
    }
  }

  return 100.*(float) flip / (float) nonzero;
}

template <class T>
float trials(mvector<T> &series, bool &mask) {
  int i, nonzero = 0, flip = 0, follow = 0;

  nonzero = 0;
  flip = 0;
  for (i = 0; i < series.xpoints(); i++) {
    if (series[i] != 0) {
      nonzero += 1;
    }
    if (i != 0) {
      if ( ( (series[i] != 0) && (series[i-1] == 0) ) ||
           ( (series[i] == 0) && (series[i-1] != 0) )  ) {
        flip += 1;
      }
      if ( (series[i] != 0) && (series[i-1] != 0) ) {
        follow += 1;
      }
    }
  } //for

  if (flip < nonzero - sqrt(nonzero*(1.-nonzero/series.xpoints()) ) ) {
    mask = false;
  }
  else {
    mask = true;
  }
  return 100.*(float) flip / (float) nonzero;
}

