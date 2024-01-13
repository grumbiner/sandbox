
#ifndef NCEPGRIDS
  #include "ncepgrids.h"
#endif
#ifndef POINTSH
  #include "points.h"
#endif

#define LAND 157
#define NO_DATA 224
#define COAST 195
#define WEATHER 177

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
    point3<float> polarization();
    point3<float> gradient();
    point3<float> differences();
    int qc();
    void show();
};
void ssmipt::show() {
  printf("ssmipt %5d %5d %5d %5d %5d %5d %5d   %3d %3d\n",
    ssmi.t19v, ssmi.t19h,ssmi.t22v, ssmi.t37v, ssmi.t37h, ssmi.t85v, ssmi.t85h,
    ssmi.conc_bar, ssmi.bar_conc);
}
ssmipt::qc() {
  int ret=0;
// If not a data point, don't worry
  if (ssmi.conc_bar == LAND ||
      ssmi.conc_bar == WEATHER || 
      ssmi.conc_bar == COAST || 
      ssmi.conc_bar == NO_DATA ||
      ssmi.bar_conc == LAND ||
      ssmi.bar_conc == WEATHER || 
      ssmi.bar_conc == COAST || 
      ssmi.bar_conc == NO_DATA ) {
    return ret;
  }
  if (ssmi.t19v < 50*100 || ssmi.t19v > 300*100) {
    ret += 1;
  }
  if (ssmi.t19h < 50*100 || ssmi.t19h > 300*100) {
    ret += 1;
  }
  if (ssmi.t22v < 50*100 || ssmi.t22v > 300*100) {
    ret += 1;
  }
  if (ssmi.t37v < 50*100 || ssmi.t37v > 300*100) {
    ret += 1;
  }
  if (ssmi.t37h < 50*100 || ssmi.t37h > 300*100) {
    ret += 1;
  }
  if (ssmi.t85v < 50*100 || ssmi.t85v > 300*100) {
    ret += 1;
  }
  if (ssmi.t85h < 50*100 || ssmi.t85h > 300*100) {
    ret += 1;
  }

  return ret;
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


int main(int argc, char *argv[]) {
  northgrid<ssmipt> north;
  FILE *fin;
  ijpt x;

  fin = fopen(argv[1],"r");
  north.binin(fin);
  for (x.j = 0; x.j < north.ypoints() ; x.j++) {
  for (x.i = 0; x.i < north.xpoints() ; x.i++) {
     if (north[x].qc() != 0) {
       printf("%d failed at %3d %3d ", north[x].qc(), x.i, x.j);
       north[x].show();
     }
     if (//north[x].ssmi.conc_bar != north[x].ssmi.bar_conc &&
         (north[x].ssmi.conc_bar != WEATHER && 
          north[x].ssmi.bar_conc != WEATHER   ) &&
         (north[x].ssmi.conc_bar > 0 && 
          north[x].ssmi.bar_conc > 0   ) 
        ) {
       printf("delconc %3d %3d  %3d %3d  %4d\n",
              x.i, x.j, north[x].ssmi.conc_bar, 
              north[x].ssmi.bar_conc,          
              north[x].ssmi.conc_bar - north[x].ssmi.bar_conc);
      }
  }
  }

  return 0;
} 
