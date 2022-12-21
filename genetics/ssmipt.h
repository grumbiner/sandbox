#ifndef SSMIPT_H
#define SSMIPT_H
#include "mvector.h"
#include "points.h"

typedef struct {
  unsigned int t19v : 16;
  unsigned int t19h : 16;
  unsigned int t22v : 16;
  unsigned int t37v : 16;
  unsigned int t37h : 16;
  unsigned int t85v : 16;
  unsigned int t85h : 16;
  unsigned int conc_bar :  8;
  unsigned int bar_conc :  8;
} ssmi_struct;

class ssmipt {
  public:
    ssmi_struct ssmi ;
    ssmipt();
    ssmipt & operator=(int );
    void tovec(mvector<float> &);
    point3<float> polarization();
    point3<float> gradient();
    point3<float> differences();
};
void ssmipt::tovec(mvector<float> &x) {
  if (x.xpoints() != 9) {
    x.resize(9);
  }
  x[0] = ssmi.t19v/100.;
  x[1] = ssmi.t19h/100.;
  x[2] = ssmi.t22v/100.;
  x[3] = ssmi.t37v/100.;
  x[4] = ssmi.t37h/100.;
  x[5] = ssmi.t85v/100.;
  x[6] = ssmi.t85h/100.;
  x[7] = ssmi.conc_bar/100.;
  x[8] = ssmi.bar_conc/100.;
  return;
}
ssmipt::ssmipt() {
  ssmi.t19v = 0;
  ssmi.t19h = 0;
  ssmi.t22v = 0;
  ssmi.t37v = 0;
  ssmi.t37h = 0;
  ssmi.t85v = 0;
  ssmi.t85h = 0;
  ssmi.conc_bar = 0;
  ssmi.bar_conc = 0;
}
ssmipt & ssmipt::operator=(int x) {
  ssmi.t19v = x;
  ssmi.t19h = x;
  ssmi.t22v = x;
  ssmi.t37v = x;
  ssmi.t37h = x;
  ssmi.t85v = x;
  ssmi.t85h = x;
  ssmi.conc_bar = x;
  ssmi.bar_conc = x;
  return *this;
}
point3<float> ssmipt::gradient() {
  point3<float> x;
  x.i = (float) ssmi.t22v / (float) ssmi.t19v;
  x.j = (float) ssmi.t37v / (float) ssmi.t19v;
  x.k = (float) ssmi.t85v / (float) ssmi.t19v;
  return x;
}
point3<float> ssmipt::polarization() {
  point3<float> x;
  x.i = (float) ssmi.t19v / (float) ssmi.t19h;
  x.j = (float) ssmi.t37v / (float) ssmi.t37h;
  x.k = (float) ssmi.t85v / (float) ssmi.t85h;
  return x;
}
point3<float> ssmipt::differences() {
  point3<float> x;
  x.i = (float) ssmi.t19v - (float) ssmi.t19h;
  x.j = (float) ssmi.t37v - (float) ssmi.t37h;
  x.k = (float) ssmi.t85v - (float) ssmi.t85h;
  return x;
}

#endif
