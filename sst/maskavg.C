#include "ncepgrids.h"

void reducer(llgrid<unsigned char> &x, llgrid<unsigned char> &y, int n) ;
void maskanaly(llgrid<unsigned char> &x, float &land, float &water, float &coast);
template <class T>
void cellarea(llgrid<T> &x, int n) ;

int main(void) {
  FILE *fin, *fout;
  mvector<int> scale(32);
  global_12th<unsigned char> mask;
  float land, water, coast;
  float lat, lon;
  int i, n;
  ijpt loc;

  fin = fopen("seaice_gland5min","r");
  mask.binin(fin);
  fclose(fin);
  maskanaly(mask, land, water, coast);

  scale[0] = 1;
  scale[1] = 2;
  scale[2] = 3;
  scale[3] = 4;
  scale[4] = 5;
  scale[5] = 6;
  scale[6] = 8;
  scale[7] = 9;
  scale[8] = 10;
  scale[9] = 12;
  scale[10] = 15;
  scale[11] = 18;
  scale[12] = 20;
  scale[13] = 24;
  scale[14] = 27;
  scale[15] = 30;
  scale[16] = 36;
  scale[17] = 40;
  scale[18] = 45;
  scale[19] = 48;
  scale[20] = 54;
  scale[21] = 60;
  scale[22] = 72;
  scale[23] = 80;
  scale[24] = 90;
  scale[25] = 108;
  scale[26] = 120;
  scale[27] = 135;
  scale[28] = 144;
  scale[29] = 216;
  scale[30] = 240;
  scale[31] = 270;

  for (i = 0; i < scale.xpoints(); i++) {
    llgrid<unsigned char> *reduced_mask;
    n = scale[i];
    if (mask.dlat < 0) { // starting near north pole
      lat =  90 + n*mask.dlat/2.;
    }
    else {
      lat = -90 + n*mask.dlat/2.;
    }
    lon = n*mask.dlon/2.;
    reduced_mask = new llgrid<unsigned char>(mask.xpoints()/n, mask.ypoints()/n, 
                              mask.dlat*n, mask.dlon*n, lat, lon);
    cellarea(*reduced_mask, n);
    
    reducer(mask, *reduced_mask, n ) ;
    maskanaly(*reduced_mask, land, water, coast);
    printf("ratio %2d maskanaly: %f %f %f  %f\n",n, land/1.e12, water/1.e12, coast/1.e12, (land+water+coast)/1.e12);
    delete reduced_mask;
  }

  return 0;
}
void reducer(llgrid<unsigned char> &x, llgrid<unsigned char> &y, int n) {
  float lat, lon;
  ijpt loc, tloc;
  int i, j;
  llgrid<int> *count;

  if (x.dlat < 0) { // starting near north pole
    lat = 90 - n*x.dlat/2.;
  }
  else {
    lat = -90 + n*x.dlat/2.;
  }
  lon = n*x.dlon/2.;

  count = new llgrid<int>(x.xpoints()/n, x.ypoints()/n, x.dlat*n, x.dlon*n,
                                lat, lon);
  y.set(0);
  count->set(0);

  for (loc.j = 0; loc.j < y.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < y.xpoints(); loc.i++) {
    for (j = 0; j < n; j++) {
    for (i = 0; i < n; i++) {
      tloc.j = loc.j*n + j;
      tloc.i = loc.i*n + i;
      if (x[tloc] != 0) { // count up non-water points
        count->operator[](loc) += 1;
      }
    }
    }
    if (count->operator[](loc) > 0 ) {
      if (count->operator[](loc) < n*n) {
        y[loc] = 195;
      }
      else {
        y[loc] = 157;
      }
    }
    if (y.xpoints() < 50) {
       printf("loc %d %d y = %d\n",loc.i, loc.j,y[loc] ); fflush(stdout);
    }
  }
  }
  //printf("leaving reducer\n"); fflush(stdout);

  return;
} 
void maskanaly(llgrid<unsigned char> &x, float &land, float &water, float &coast) {
  ijpt loc;
  double dland=0, dwater=0, dcoast = 0;
  //printf("entered maskanaly nx ny = %d %d \n", x.xpoints(), x.ypoints() ); fflush(stdout);

  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (x.xpoints() < 50) printf("%d %d %d %f\n",loc.i, loc.j, x[loc], x.cellarea(loc) ); fflush(stdout);
    if (x[loc] == 0) {
      dwater += x.cellarea(loc);
    }
    else if (x[loc] == 157) {
      dland += x.cellarea(loc);
    }
    else if (x[loc] == 195) {
      dcoast += x.cellarea(loc);
    }
  }
  }
  land = (float) dland;
  water = (float) dwater;
  coast = (float) dcoast;
  //printf("%7.3f %7.3f %7.3f\n",land/1.e12, water/1.e12, coast/1.e12);

  return;
}

template <class T>
void cellarea(llgrid<T> &x, int n) {
  ijpt loc;
  latpt ll;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (x.cellarea(loc) < 0) {
      ll = x.locate(loc);
      printf("%d  %d %d  %f %f  %f\n",n, loc.i, loc.j, ll.lon, ll.lat, x.cellarea(loc) );
    }
  }
  }
}
