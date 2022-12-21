
#include "ncepgrids.h"

#define NMONTHS 216
#define PREDMONTHS 71
#define LIM 2.5

int gridin(FILE *fin, mvector<GRIDTYPE<float> > &x) ;
float dist(ijpt &x, ijpt &y);
void tovec( mvector<GRIDTYPE<float> > &x, float *y, ijpt &loc);

extern void  correl(float *a1, float *a2, const int k, 
              float *r2, float *xbar, float *ybar, float *sig2x, float *sig2y);
extern float sumx(float *x, const int n);
extern float sumxy(float *x, float *y, const int n);
extern float sumx2(float *x, const int n);

float makepred(mvector<GRIDTYPE<float> > &xgrid, mvector<float> &soi, 
                int lag, ijpt loc, float *x, float *y);

int main(int argc, char *argv[]) {
  mvector<GRIDTYPE<float> > alldata(NMONTHS+PREDMONTHS); 
  mvector<GRIDTYPE<float> > climo(12); 
  mvector<float> soi(NMONTHS+PREDMONTHS + 1);
  GRIDTYPE<float> corfig, land;
  GRIDTYPE<int> count;
  GRIDTYPE<unsigned char> yland;
  float flag;
  FILE *fin;
  float x[NMONTHS], y[NMONTHS], ylag[NMONTHS];
  float xfcst[NMONTHS], yfcst[NMONTHS];
  int i, j, totpts = 0, lag, fcst;
  ijpt loc;
  latpt ll;
  float r2, xbar, ybar, sig2x, sig2y;
  palette<unsigned char> gg(19,65);
  char fname[900];

  fin = fopen(argv[1],"r");
  gridin(fin, alldata);
  fclose(fin);
  for (i = 0; i < 12; i++) {
    climo[i].set((float) 0.);
  }
  for (i = 0; i < NMONTHS; i++) {
    climo[i%12] += alldata[i];
  }
  for (i = 0; i < 12; i++) {
    climo[i] /= ((float) NMONTHS)/12.;
  }

  // Subtract out the climatological field by months:
  for (i = 0; i < NMONTHS; i++) {
    alldata[i] -= climo[i%12];
  }

  // Get land, and then flag out all points that also never have ice
  fin = fopen(argv[2],"r");
  yland.binin(fin);
  fclose(fin);
  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
     land[loc] = yland[loc];
     if (land[loc] > 157 ) land[loc] = 157;
  }
  }
  land.xpm("land.xpm",9,gg);
  //printf("land max, min, avg = %f %f %f\n",
  //  (float) land.gridmax(), (float) land.gridmin(), (float) land.average() );
  flag = land.gridmax();
  count.set(0);
  for (i = 0; i < NMONTHS; i++) {
    //printf("%d gridmin %f max average %f %f \n",i, alldata[i].gridmin(), 
    //             alldata[i].gridmax(), alldata[i].average() );
    for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
      if (alldata[i][loc] != 0) count[loc] += 1;
    }
    }
  }
  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
    //printf("%3d %3d  %d %f %f %d\n",loc.i, loc.j, count[loc], land[loc], flag, flag==land[loc]);
    if (count[loc] == 0 ) {
      land[loc] = flag; 
    }
    else {
      if (land[loc] != flag) totpts += 1;
    }
  }
  }
  printf("There were a total of %d non-land, sometimes-covered points\n",totpts);
  fflush(stdout);
  if (totpts > land.xpoints()*land.ypoints() / 2) {
    printf("There must be some kind of error!\n");
    return 1;
  }

  // Read in the soi mvector
  fin = fopen(argv[3],"r");
  for (i = 0; i < soi.xpoints()/12; i++) {
    int idum; float fdum;
    //fscanf(fin, " %d %f %f %f %f %f %f %f %f %f %f %f %f %f\n",&idum,
    fscanf(fin, " %d %f %f %f %f %f %f %f %f %f %f %f %f\n",&idum,
      &soi[i*12+0], &soi[i*12+1], &soi[i*12+2], &soi[i*12+3],
      &soi[i*12+4], &soi[i*12+5], &soi[i*12+6], &soi[i*12+7],
      &soi[i*12+8], &soi[i*12+9], &soi[i*12+10], &soi[i*12+11]
      //,&fdum);
      );
    //printf("%d\n",idum); fflush(stdout);
    //for (j = 0; j < 12; j++ ) {
    //  y[j+12*i] = soi[j+12*i];
    //}
  }
  fclose(fin);

  //soi -= soi.average();
  for (i = 0; i < NMONTHS; i++) {
    //y[i] = copysign(sqrt(fabs(soi[i])), soi[i]);
    //if (soi[i] > LIM) soi[i] = LIM;
    //if (soi[i] < -LIM) soi[i] = -LIM;
    //y[i] = soi[i];
    soi[i] = copysign(soi[i]*soi[i], soi[i]);
    y[i] = soi[i];
    //printf("%3d %f\n",i, y[i]);
  }
  fflush(stdout);

///////////////////////////////////////////////////////////////////////
// Now try looping over all points and computing correlations with soi
  for (lag = 0; lag < 63; lag++) {
    for (j = 0; j < NMONTHS; j++) {
       ylag[j] = y[(j+lag)%NMONTHS];
    }
    corfig.set((float) 0.);
    for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
       if (land[loc] != flag) {
         tovec(alldata, x, loc);
         correl(x, ylag, NMONTHS, &r2, &xbar, &ybar, &sig2x, &sig2y);
         if (sig2x > 0 && r2*r2 >= 0.01) {
           ll = corfig.locate(loc);
           printf("%2d %3d %3d %5.1f %6.1f  %5.3f %f %f\n",lag, loc.i, loc.j, 
                          ll.lat, ll.lon, r2*r2, xbar, sig2x);
           corfig[loc] = r2*r2;
         }
       }
    }
    }
    printf("%2d Maximum, avg r^2 %f %f\n",lag, corfig.gridmax(), 
                corfig.average((float) 0.0) ); fflush(stdout);
    // For all good candidates, make and score prediction:
    for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
       if (corfig[loc] > 0.125) {
         printf("pred %3d %3d %02d %5.3f %5.3f\n",loc.i, loc.j, lag, 
              corfig[loc], 
              makepred(alldata, soi, lag, loc, xfcst, yfcst)  ) ;
         if (makepred(alldata, soi, lag, loc, xfcst, yfcst) > 0.125) {
           for (fcst = 0; fcst < PREDMONTHS; fcst++) {
              printf("fc %02d %3d %3d  %3d %f %f\n",lag, loc.i, loc.j, fcst, 
                     xfcst[fcst], yfcst[fcst] );
           }
         }
       }
    }
    }
    corfig /= corfig.gridmax();
    corfig *= 18;
    sprintf(fname,"cor%02d.xpm",lag);
    corfig.xpm(fname,1,gg);
  }
  printf("Preparing to shutdown program \n"); fflush(stdout);

  return 0;
}

int gridin(FILE *fin, mvector<GRIDTYPE<float> > &x) {
  int i;
  ijpt loc;
  for (i = 0; i < x.xpoints(); i++) {
    x[i].binin(fin);
    for (loc.j = 0; loc.j < x[0].ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < x[0].xpoints(); loc.i++) {
      if (x[i][loc] < 0) x[i][loc] = 0;
      if (x[i][loc] > 200) x[i][loc] = 0;
    }
    }
    //printf("in gridin, max, min, average %f %f %f\n",x[i].gridmax(), 
    //             x[i].gridmin(), x[i].average() );
  }
  return i;
}
// Note that it would be better to do true geometric distances rather than
//   number of grid points, especially when we get to non-polar-stereo grids.
float dist(ijpt &x, ijpt &y) {
  return sqrt( (x.i-y.i)*(x.i-y.i) + (x.j - y.j)*(x.j-y.j) );
}
void tovec( mvector<GRIDTYPE<float> > &x, float *y, ijpt &loc) {
  int i;
  for (i = 0; i < x.xpoints(); i++) {
    y[i] = x[i][loc];
  }
  return;
}
float makepred(mvector<GRIDTYPE<float> > &all, mvector<float> &soi, 
                int lag, ijpt loc, float *x, float *y) {
  int i, n = NMONTHS;
  float r2, xbar, ybar, sig2x, sig2y;
  float sx, sy, sxy, sx2, sy2;
  float a, b, delta;
  

  // Build training set:
  for (i = 0 ; i < n; i++) {
     x[i] = all[i][loc];
     y[i] = soi[(i+lag)% n ];
  }
  sx = sumx(x, n);
  sy = sumx(y, n);
  sxy = sumxy(x, y, n);
  sx2 = sumx2(x, n); 
  sy2 = sumx2(y, n); 
  delta = n*sx2 - sx*sx;
  if (delta <= 0.0 || n*sx2-sx*sx <= 0 || n*sy2-sy*sy <= 0) {
    printf("delta err %f %f %f\n",delta, n*sx2-sx*sx, n*sy2-sy*sy);
    return 0;
  }
  // y = ax + b
  a     = (n*sxy - sx*sy)/delta;
  b     = (sx2*sy - sx*sxy)/delta;
 
  for (i = 0 ; i < PREDMONTHS; i++) {
     x[i] = a * all[NMONTHS+i-lag][loc] + b;
     y[i] = soi[i + NMONTHS];
  }
  correl(x, y, PREDMONTHS, &r2, &xbar, &ybar, &sig2x, &sig2y);

  return r2*r2;
}
