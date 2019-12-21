#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "mvector.h"

template <class T>
void shuffle(mvector<T> &source, mvector<T> &dest, int seed);
template <class T>
float bootstrap(mvector<T> &x, mvector<T> &y) ;

template <class T>
float dot(mvector<T> &x, mvector<T> &y) ;

int main(void) {
  mvector<float> test(20), out(20);
  int i;
  
  for (i = 0; i < test.xpoints() ; i++) {
    test[i] = (float) i;
  }

  shuffle(test, out, 0);
  shuffle(out, out, 0);
  test -= test.average() ;
  out -= out.average() ;

  for (i = 0; i < test.xpoints() ; i++) {
    printf("%2d %6.3f %6.3f\n",i, test[i], out[i]);
  }
  printf("bootstrap return %f\n",bootstrap(test, out) );

  return 0;
}
//Shuffle a mvector, returning result in another mvector
template <class T>
void shuffle(mvector<T> &source, mvector<T> &dest, int seed) {
  int nrem = source.xpoints();
  int i, count=0;
  mvector<T> orig(source.xpoints() );

  //Transfer source data to a temporary mvector which we will
  // modify
  orig = source;

  //Initialize the pseudorandom number generator
  srand(seed);
  while (nrem > 0) {
    i = (int) ((float)nrem * rand() / (RAND_MAX + 1.0) );
    dest[count] = orig[i];
    orig[i] = orig[nrem-1]; //Bring the end of series value down to 'i'
    nrem  -= 1;
    count += 1;
  }

  return;
}

//Using the above 'shuffle', take two mvector find their correlation,
//  and then find the % of the time that a reshuffling beats that
//  correlation.  Assume that the mean has been removed already.
template <class T>
float bootstrap(mvector<T> &x, mvector<T> &y) {
  float sd1, sd2, corref;
  int i, count= 0, maxcount=1000 ;
  mvector<T> z(x.xpoints() );

  sd1 = x.norm(2);
  sd2 = y.norm(2);
  if (sd1*sd2 == 0.) return -1.;
  corref = fabs(dot(x, y) / (sd1*sd2));
  for (i = 0; i < maxcount; i++) {
    shuffle(y, z, i); //last is seed, which must vary between calls
    sd2 = z.norm(2);
    if (fabs(dot(x,z) / (sd1*sd2) ) > corref) {
      count += 1;
    }
  }

  return (float) count / (float) maxcount;
}
template <class T>
float dot(mvector<T> &x, mvector<T> &y) {
  int i;
  float sum = 0.0;
  if (x.xpoints() != y.xpoints() ) {
    printf("cannot dot unequal sized mvector\n");
    return -1.;
  }
  for (i = 0; i < x.xpoints() ; i++) {
    sum += x[i]*y[i];
  }
  return sum;
}

