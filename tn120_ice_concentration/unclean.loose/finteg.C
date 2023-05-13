#include <stdio.h>
#include "ncepgrids.h"

template <class T>
class highll : public llgrid<T> {
  public:
    highll();
};
template <class T>
highll<T>::highll() {
  nx = 360 * 1;
  ny = 180 * 1;
  dlat = -1.0;
  dlon =  1.0;
  firstlon = dlon / 2.;
  firstlat = 90.0 + dlat/2.;
  grid = new T[nx*ny];
}

#define MAX_CONC 126
#define DESTTYPE highll

void newfilt(GRIDTYPE<float> &nf, GRIDTYPE<float> &nlf, 
                DESTTYPE<float> &glob) ;
void operfilt(GRIDTYPE<float> &nf, GRIDTYPE<float> &nlf, 
                DESTTYPE<float> &glob) ;
int main(int argc, char *argv[]) {
  FILE *fin;
  GRIDTYPE<unsigned char> nland;
  GRIDTYPE<float> nf, nf2, nlf;
  DESTTYPE<float> glob, glob2, glob3;
  float area_base, area1, area2, area3;
  ijpt ll;
  palette<unsigned char> gg(19, 65);

//Get data files
  fin = fopen(argv[1],"r");
  nf.binin(fin);
  fclose(fin);
  fin = fopen(argv[2],"r");
  nland.binin(fin);
  fclose(fin);

// Ensure percent scaling.
  if (nf.average() < 2.56) nf *= 100.;

  for (ll.j = 0; ll.j < nf.ypoints() ;ll.j++) {
  for (ll.i = 0; ll.i < nf.xpoints() ; ll.i++ ) {
    if (nf[ll] > MAX_CONC || nland[ll] > 100) { 
      nf[ll] = 0;
    }
    if (nf[ll] > 100) {
      nf[ll] = 100;
    }
    nlf[ll] = (float) nland[ll];
  }
  }

  nf.xpm(argv[3], 7, gg);
  area_base = nf.integrate()/1.e12 / 100.;

// Method 1: Simple interpolation.
  glob.fromall(nf, nlf, 157., 0.); 
  area1 = glob.integrate() / 1.e12 / 100.;
  glob.xpm("glob1.xpm", 7, gg);

// Method 2: Averaging plus 'on demand' interpolation.
  operfilt(nf, nlf, glob2);
  area2 = glob2.integrate() / 1.e12 / 100.;
  glob2.xpm("glob2.xpm", 7, gg);

// Method 3: Area weighted averaging plus 'on demand':
//  newfilt(nf, nlf, glob3);
//  area3 = glob3.integrate() / 1.e12 / 100.;
//  glob3.xpm("glob3.xpm", 7, gg);

  printf("area orig %8.4f method1 %8.4f method2 %8.4f method3 %8.4f\n", 
                  area_base, area1, area2, area3);
// Execute return interpolation
// Method 1: Simple interpolation.
  glob2.set(0.0);
  nf2.fromall(glob, glob2, 157., 0.); 
  area1 = nf2.integrate() / 1.e12 / 100.;
  printf("Area of return interpolation %f\n",area1); 
  nf2.xpm("nf2.xpm",7,gg);


  return 0;
}
void operfilt(GRIDTYPE<float> &nf, GRIDTYPE<float> &nlf, 
                                     DESTTYPE<float> &glob) {
  latpt lloc;
  ijpt  ijloc;
  fijpt floc;
  DESTTYPE<int> count;

  count.set(0);
  glob.set(0.0);
// Direct transfer for averaging of the native grid concentrations
  for (ijloc.j = 0; ijloc.j < nf.ypoints() ; ijloc.j++) {
  for (ijloc.i = 0; ijloc.i < nf.xpoints() ; ijloc.i++) {
    if (nf[ijloc] < MAX_CONC && nlf[ijloc] < MAX_CONC) {
      lloc = nf.locate(ijloc);
      floc = glob.locate(lloc);
      glob[floc] += min((float)100., nf[ijloc]);
      count[floc] += 1;
    }
  }
  }

// Handle the averaging
  for (ijloc.j = 0; ijloc.j < count.ypoints() ; ijloc.j++) {
  for (ijloc.i = 0; ijloc.i < count.xpoints() ; ijloc.i++) {
    if (count[ijloc] != 0) {
      glob[ijloc] /= count[ijloc] ;
    }
    else {
      // Take nearest neighbor
      lloc = glob.locate(ijloc);
      floc = nf.locate(lloc);
      if ( nf.in(floc) ) {
        if (nf[floc] < MAX_CONC) {
          glob[ijloc] = min((float) 100, nf[floc]) ;
          count[ijloc] = 1;
        }
      }
    } 
  }
  }

  return;
}
void newfilt(GRIDTYPE<float> &nf, GRIDTYPE<float> &nlf, 
                                     DESTTYPE<float> &glob) {
  latpt lloc;
  ijpt  ijloc;
  fijpt floc;
  float tmp;
  DESTTYPE<int> count;

  count.set(0);
  glob.set(0.0);
// Direct transfer for averaging of the native grid concentrations
  for (ijloc.j = 0; ijloc.j < nf.ypoints() ; ijloc.j++) {
  for (ijloc.i = 0; ijloc.i < nf.xpoints() ; ijloc.i++) {
    if (nf[ijloc] < MAX_CONC && nlf[ijloc] < MAX_CONC) {
      lloc = nf.locate(ijloc);
      floc = glob.locate(lloc);
      tmp = min((float)100., nf[ijloc]);
      glob[floc] += nf.cellarea(ijloc)*tmp;
      count[floc] += 1;
    }
  }
  }

// Handle the averaging
  for (ijloc.j = 0; ijloc.j < count.ypoints() ; ijloc.j++) {
  for (ijloc.i = 0; ijloc.i < count.xpoints() ; ijloc.i++) {
    if (count[ijloc] != 0) {
      glob[ijloc] /= glob.cellarea(ijloc);
    }
    else {
      // Take nearest neighbor
      lloc = glob.locate(ijloc);
      floc = nf.locate(lloc);
      if ( nf.in(floc) ) {
        if (nf[floc] < MAX_CONC) {
          glob[ijloc] = min((float) 100, nf[floc]) ;
          count[ijloc] = 1;
        }
      }
    } 
  }
  }

  if (glob.gridmax() > 100.) {
    printf("Managed to produce a gridmax over limit, finding points\n");
    for (ijloc.j = 0; ijloc.j < glob.ypoints() ; ijloc.j++) {
    for (ijloc.i = 0; ijloc.i < glob.xpoints() ; ijloc.i++) {
      if (glob[ijloc] > 100.) {
        lloc = glob.locate(ijloc);
        printf("%6.3f %6.3f  %6.2f %4d \n",lloc.lon, lloc.lat, glob[ijloc], count[ijloc]); fflush(stdout);
      }
    }
    }
  }

  return;
}
