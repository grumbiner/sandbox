#include <stdio.h>
#include "grid3.h"
#include "ncepgrids.h"
#include "cofs.h"

// Program to work on conversion between CFS native grid and a 
//   regular lat-long grid.
// Robert Grumbine 29 October 1997 

//The following takes a cfsmetric and a lat-long point and returns the
//  interpolated/estimated location in i-j space on the cfs grid of
//  that point.
fijpt finder(cfslevel<float> *x, latpt &y) ;

int main(int argc, char *argv[]) {

  cfslevel<float> cfsmetric;

// CFS-related data:
  cfsgrid3<float> t3cfs(19), s3cfs(19);

  cfsreg<float> tempor;

// Utility variables:
  latpt alat;
  ijpt x;
  fijpt y;
  FILE *fin, *fout;
  char fname[600];
  int i, j, k;
  float nonval = -99.9, maskval = 0.0;

// Check lat-long positions of the corner points:
//  x.i = 0;
//  x.j = 0;
//  alat = cfsmetric.locate(x);
//  printf("Location of 00        point: %f %f \n", alat.lon, alat.lat);
//
//  x.i = t3cfs.xpoints() - 1;
//  alat = cfsmetric.locate(x);
//  printf("Location of xpoints()-1, 0   point: %f %f \n", alat.lon, alat.lat);
//  
//  x.j = t3cfs.ypoints() - 1;
//  alat = cfsmetric.locate(x);
//  printf("Location of xpoints()-1,ypoints()-1 point: %f %f \n", alat.lon, alat.lat);
//
//  x.i = 0;
//  alat = cfsmetric.locate(x);
//  printf("Location of 0, ypoints()-1   point: %f %f \n", alat.lon, alat.lat);
//
  fout = fopen("cfsbinary", "w");
  cfsmetric.z.binout(fout);
  cfsmetric.alat.binout(fout);
  cfsmetric.alon.binout(fout);
  cfsmetric.depth.binout(fout);
  fclose(fout);

  for (j = 0; j < tempor.ypoints(); j++) {
  x.j = j;
  for (i = 0; i < tempor.xpoints(); i++) {
    x.i = i;
    alat = tempor.locate(x);
    y    = finder(&cfsmetric, alat);
    printf(" - %3d %3d point: %6.3f %6.3f  %1d  %6.3f %6.3f\n", i, j, alat.lon, alat.lat,
       cfsmetric.llin(alat) , y.i, y.j);
  }
  }

  return 0;
  
}

fijpt finder(cfslevel<float> *x, latpt &y) {
  fijpt bestloc, ll, lr, ul, ur, est, est1, est2, delta;
  latpt llll, lllr, llul, llur, llest;
  latpt llorig ;

  llorig = y;
  if (llorig.lon < 0.) llorig.lon += 360.;
// Set up initial bounding i,js
  ll.i = 0.0;
  ll.j = 0.0;
  lr.i = x->depth.xpoints() - 1;
  lr.j = 0.0;
  ur.i = x->depth.xpoints() - 1;
  ur.j = x->depth.ypoints() - 1;
  ul.i = 0.0;
  ul.j = x->depth.ypoints() - 1;

// find ll coords of the bounding points
  llll = x->locate(ll);
  lllr = x->locate(lr);
  llul = x->locate(ul);
  llur = x->locate(ur);
  if (llll.lon < 0.) llll.lon += 360.;
  if (lllr.lon < 0.) lllr.lon += 360.;
  if (llur.lon < 0.) llur.lon += 360.;
  if (llul.lon < 0.) llul.lon += 360.;


    bestloc.i = -1.0;
    bestloc.j = -1.0;
// if we're inside the extreme bounding box, try to find a location.
  if (x->llin(llorig) ) { 
    delta.i = llorig.lon - llll.lon;
    delta.j = llorig.lat - llll.lat;
    est1.i = delta.i / (llur.lon - llll.lon)  * lr.i;
    est1.j = delta.j / (llur.lat - llll.lat)  * ur.j;
    delta.i = llorig.lon - llul.lon;
    if (llorig.lat > llul.lat ) {
       delta.j = llul.lat - lllr.lat; 
    }
    else if (llorig.lat < lllr.lat ) {
       delta.j = 0.0;
    }
    else {
      delta.j = llorig.lat - lllr.lat; 
    }
    est2.i =   delta.i / (lllr.lon - llul.lon)  * lr.i;
    est2.j =   delta.j / (llul.lat - lllr.lat)  * ul.j;
    est.i = (est1.i + est2.i ) / 2.;
    est.j = (est1.j + est2.j ) / 2.;
//  First iteration/pass.  Large scale interpolated/estimated location.
    llest = x->locate(est); 
    if (llest.lat < -90.0) {
      //return bestloc;
    }
    printf("%6.3f %6.3f %6.3f %6.3f ", est1.j, est2.j, llest.lon + 360., llest.lat); 
    return ( est );
    
  }
  else {
    bestloc.i = -1.0;
    bestloc.j = -1.0;
  }

  return bestloc;
}
