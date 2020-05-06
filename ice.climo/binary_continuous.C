//30 July 1999
#include "mvector.h"

//Given a binary series and a continuous series, test differences between
//  the 'on' and 'off' times, returning a bootstrap probability of the
//  difference being this large or larger.

extern float partition(mvector<int> &ice, mvector<float> &soi) ;
template <class T>
extern void shuffle(mvector<T> &source, mvector<T> &dest, int seed) ;

float binary_continuous(mvector<int> &ice, mvector<float> &soi) ;

float binary_continuous(mvector<int> &ice, mvector<float> &soi) {
  int i, count=0, maxcount = 1000;
  mvector<float> tmp(soi.xpoints() );
  float delta, deltaref;
  
  //find reference value:
  deltaref = partition(ice, soi);
  for (i = 0; i < maxcount; i++) {
    shuffle(soi, tmp, i);
    delta = partition(ice, tmp);
    if ( fabs(delta) > fabs(deltaref) ) {
      count += 1;  // delta is bigger than ref, ignore sign
    }
  }

  return (float) count / (float) maxcount;
}

