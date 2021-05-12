#include "ncepgrids.h"

#define ARCTIC 0
#define ANTARCTIC 1

double ice_extent(GRIDTYPE<float> &x, float flag) ;
double area(GRIDTYPE<float> &x, float flag);
void mask(GRIDTYPE<float> &x, GRIDTYPE<float> &y, int region) ;

int main(int argc, char* argv[]) {
  FILE *fin;
  global_ice<unsigned char> icein, skip, tmp2;
  global_ice<float> sum, tmp, average;
  double arctic_extent = 0, arctic_area = 0;
  double antarctic_extent = 0, antarctic_area = 0;
  int count;

  //printf("argc = %d\n",argc); fflush(stdout);
  //for (count = 0; count < argc; count++) {
  //  printf("arg %d = %s\n",count, argv[count]);
  //  fflush(stdout);
  //}
  fin = fopen("skip","r");
  skip.binin(fin);
  fclose(fin);
// Now translate:
  tmp2.set((unsigned char) 1);
  tmp2 -= skip;
  skip = tmp2;

  for (count = 1; count < argc; count++) {
    fin = fopen(argv[count], "r");
    if (fin == (FILE *) NULL) {
      printf("failed to open %s\n",argv[count]);
      return 1;
    }
    icein.binin(fin);
    fclose(fin);
    icein *= skip;

    conv(icein, average);
    if (average.gridmax() < 5) average *= 100.;
    mask(average, tmp, ARCTIC);
    arctic_extent += ice_extent(tmp, 157.);
    arctic_area   += area(tmp, 157.);

    mask(average, tmp, ANTARCTIC);
    antarctic_extent += ice_extent(tmp, 157.);
    antarctic_area   += area(tmp, 157.);
     
    sum += average;
  }
  count -= 1;
  //printf("count = %d\n",count); fflush(stdout);
  sum /= count;

  printf("arctic %15.8f %15.8f  ",arctic_extent / count / 1.e12, arctic_area / count / 1.e12);
  printf("antarctic %15.8f %15.8f\n",antarctic_extent / count / 1.e12, antarctic_area / count / 1.e12);
  
  return 0;
 
}

double ice_extent(GRIDTYPE<float> &x, float flag) {
  GRIDTYPE<float> tmp;
  ijpt loc;
  float minconc = 0;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (x[loc] > minconc && x[loc] <= 100. && x[loc] != flag) {
      tmp[loc] = 1.0;
    }
    else {
      tmp[loc] = 0.0;
    }
  }
  }
  return tmp.integrate();
}
double area(GRIDTYPE<float> &x, float flag) {
  GRIDTYPE<float> tmp;
  ijpt loc;
  float div = 1.;
  if (x.gridmax() > 5.) { div = 100.;}

  tmp = x;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (x[loc] == flag) {
      tmp[loc] = 0.0;
    }
  }
  }
  return tmp.integrate() / div;
}
void mask(GRIDTYPE<float> &x, GRIDTYPE<float> &y, int region) {
// Mask out points that are not in the given region, for doing
//   area/extent computations
  ijpt loc;
  latpt ll;

  y = x;

  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {

    ll = x.locate(loc);
    if (region == ARCTIC) {
      if (ll.lat < 0) y[loc] = 0.;
    }
    else if (region == ANTARCTIC) {
      if (ll.lat > 0) y[loc] = 0.;
    }
    else {
      printf("invalid region %d\n",region);
      break;
    }

  }
  }

  return;
}
