#include <stdio.h>
#include <stdlib.h>

// Demo program to try to work with NIC Mercator Grid
//   10 December 1997
//   Robert Grumbine
// Revised and renewed 18 June 2002

#include "ncepgrids.h"

template <class T>
class great_lakes_12th : public llgrid<T> {
  public:
    great_lakes_12th();
};
template <class T>
great_lakes_12th<T>::great_lakes_12th() {
  float  degreefrac = 12.;
  this->dlat = -1./degreefrac;
  this->dlon =  1./degreefrac;
// Note that the following four are where we diverge from global grid
  this->firstlon =  265. + this->dlon / 2.;
  this->firstlat =   52. + this->dlat / 2.;
  this->nx = (int) (0.5 +  50.*degreefrac);
  this->ny = (int) (0.5 +  12.*degreefrac);
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
  this->pds.set_gridid(255);
                                                                                 
  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) {cout << "failed to new a great_lakes_12th\n"; }
                                                                                 
}

void nicread(FILE *fin, mercator<float> &x ) ;
void findcorresp(global_12th<float> &x, mercator<point3<float> > &corresp) ;


int main(int argc, char *argv[]) {
  mercator<float> x, mask;
  mercator<point3<float> > corresp;

  global_12th<float> iceout, sum;
  global_12th<short int> count, pcount;
  ijpt xij, yij, iloc;
  fijpt floc;
  latpt ll;
  float nonval = -99., maskval = -1.;
  FILE *fin, *fout;
  palette<unsigned char> gg(19,65);
  float undefined = 224.;


  printf("great lakes grid, nx, ny = %d %d\n",sum.xpoints(), sum.ypoints() );
  xij.i = 0;
  xij.j = 0;
  ll = sum.locate(xij); 
  printf("0,0 = %f %f\n",ll.lat, ll.lon);
  xij.i = sum.xpoints()-1;
  xij.j = sum.ypoints()-1;
  ll = sum.locate(xij); 
  printf("nx,ny = %f %f\n",ll.lat, ll.lon);

  fin = fopen(argv[1], "r");
  if (fin == NULL) { 
    printf("Failed to open input file %s\n",argv[1]);
    return -1;
  }
  nicread(fin, x);
  fclose(fin);

  count.set(0);
  pcount.set(0);

  iceout.set((float) undefined);
  sum.set((float) 0.);

  printf("finished with nicread and other initialization, about to call findcorresp\n");
  fflush(stdout);
  findcorresp(iceout, corresp);

  for (xij.j = 0; xij.j < x.ypoints(); xij.j++) {
  for (xij.i = 0; xij.i < x.xpoints(); xij.i++) {
    floc = corresp[xij];
    iloc.i = (int) (floc.i + 0.5);
    iloc.j = (int) (floc.j + 0.5);
    if (iceout.in(floc)) {
      pcount[iloc] += 1;
      if (x[xij] == maskval || x[xij] == nonval) {
      }
      else {
        sum[iloc] += x[xij];
        count[iloc] += 1;
      }
    }
  }
  }
  for (xij.j = 0; xij.j < iceout.ypoints(); xij.j++) {
  for (xij.i = 0; xij.i < iceout.xpoints(); xij.i++) {
    if (count[xij] != 0) {
      iceout[xij] = sum[xij] / (float) count[xij];
    }
    else if (pcount[xij] != 0) {
      iceout[xij] = 0;
    }
    else {
      iceout[xij] = undefined;
    }
  }
  }

  printf("max count, pcount: %d %d\n",pcount.gridmax(), count.gridmax() );

  iceout.xpm(argv[2], 13, gg);
  fout = fopen(argv[4], "w");
  iceout.binout(fout);

  for (xij.j = 0; xij.j < x.ypoints(); xij.j++) {
  for (xij.i = 0; xij.i < x.xpoints(); xij.i++) {
    if (x[xij] == nonval) x[xij] = 157;
  }
  }
  x.xpm(argv[3], 9, gg);

  return 0;

}

void nicread(FILE *fin, mercator<float> &x ) {
// routine to decode the nic files to ice concentrations
  int maxlen=1050*2*5;
  int nichead=6; //size of the nic header
  char *strtmp;
  int i, j;
  float tmpx;

  #ifdef VERBOSE
    printf("maxlen = %d\n",maxlen);
  #endif
  strtmp = new char[maxlen];
  if (strtmp == NULL ) {
    cout << "failed to new temporary string\n"; fflush(stdout);
  }

  #ifdef VERBOSE
    cout << "NIC headers\n";
  #endif
  for (i = 0; i < nichead; i++) {
    fgets(strtmp, maxlen, fin);
    #ifdef VERBOSE
      printf("%s",strtmp);
    #endif
  }

  for (j = x.ypoints()-1; j >= 0; j--) {
  for (i = 0; i < x.xpoints(); i++) {
    fgets(strtmp, 4, fin);
    tmpx = atof(strtmp);
    x.grid[i + j*x.xpoints()] = tmpx;
  }
  fgets(strtmp,maxlen,fin);  // added to clear to the end of the line 
                             //   -- protect against CR, DOS control
  }
  
  return;
}


void findcorresp(global_12th<float> &x, mercator<point3<float> > &corresp) {
  global_12th<point3<float> > etaloc, dloc_di, dloc_dj;

  mercator<float> ice;
  mercator<short int> itcount;
  
  ijpt loc, merloc, ip, jp, locip, locjp;
  fijpt tijloc, tfl;
  latpt lloc, merll;

  float delta, dlat, dlon;
  float reflat = 45.0, reflon = -90; 
  int iter = 0, itmax = 20;
  float toler = 0.005; // tolerance in degrees
  point3<float> fij, delta_ij;


  printf("entered and initialized arguments in findcorresp\n"); fflush(stdout);
// Set up locations grid
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    lloc = x.locate(loc);
    etaloc[loc] = lloc;
  }
  }

// Construct derivatives grid, assume for now that we aren't
//  interested in the edges.
  ip.i = 1;  ip.j = 0;
  jp.i = 0;  jp.j = 1;
  for (loc.j = 0; loc.j < x.ypoints()-1; loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints()-1; loc.i++) {
    locip = loc; locip +=  ip;
    locjp = loc; locjp += jp;
    dloc_di[loc] = etaloc[locip]; dloc_di[loc] -= etaloc[loc];
    dloc_dj[loc] = etaloc[locjp]; dloc_dj[loc] -= etaloc[loc];
  }
  }

  printf("about to start newton \n"); fflush(stdout);
// Now start looking for the reference point -- use 2d newton method
  for (merloc.j = 0; merloc.j < corresp.ypoints(); merloc.j++) {
  for (merloc.i = 0; merloc.i < corresp.xpoints(); merloc.i++) {
     //printf("merloc = %d %d\n",merloc.i, merloc.j); fflush(stdout);
     merll = corresp.locate(merloc);
     reflat = merll.lat;
     reflon = merll.lon;
  
      if (reflon < 0) reflon += 360.;
      // If there's a prior guess, use it.  
      locip = merloc; locip -= ip;
      if (merloc.i != 0 && (itcount[locip] < itmax) ) {
         tfl = corresp[locip];
         loc.i = tfl.i;
         loc.j = tfl.j;
      }
      else {
        loc.i = x.xpoints()/2;
        loc.j = x.ypoints()/2;
      }
      
    tijloc.i = loc.i;
    tijloc.j = loc.j;
    fij = etaloc[loc];
    lloc.lat = fij.j; lloc.lon = fij.i;
    dlat = reflat - lloc.lat;
    dlon = reflon - lloc.lon;
    if (dlon < -360.) dlon += 360.;
    if (dlon > 360.) dlon -= 360.;
  
// Now begin the iteration:
    iter = 0;
    while (iter < itmax && (fabs(dlat) > toler || fabs(dlon) > toler) ) {
      delta = dloc_di[loc].i * dloc_dj[loc].j - dloc_di[loc].j*dloc_dj[loc].i;
      tijloc.i -= (-dlon*dloc_dj[loc].j + dlat*dloc_dj[loc].i) / delta;
      tijloc.j -= (-dlat*dloc_di[loc].i  + dlon*dloc_di[loc].j) / delta;
      loc.i = (int) (0.5 + tijloc.i);
      loc.j = (int) (0.5 + tijloc.j);
    
      fij = etaloc[loc];
      delta_ij = tijloc; delta_ij.i -= loc.i; delta_ij.j -= loc.j;
      fij +=  dloc_di[loc].i*delta_ij.i;
      fij +=  dloc_di[loc].i*delta_ij.i;
      fij +=  dloc_dj[loc].j*delta_ij.j;
      fij +=  dloc_dj[loc].j*delta_ij.j;
      lloc.lat = fij.j; lloc.lon = fij.i;
      dlat = reflat - lloc.lat;
      dlon = reflon - lloc.lon;
      if (dlon < -360.) dlon += 360.;
      if (dlon > 360.) dlon -= 360.;
   
      iter += 1;
    }
    corresp[merloc] = tijloc;
    itcount[merloc] = iter;

    
  }
  }

// Being a newton search, but now only in 1 direction at a time.  The
//   2d search failed to converge at some points:
// Now start looking for the reference point -- use 1d newton method
  for (merloc.j = 0; merloc.j < corresp.ypoints(); merloc.j++) {
  for (merloc.i = 0; merloc.i < corresp.xpoints(); merloc.i++) {
     if (itcount[merloc] < itmax) continue;

     iter = 0;
     merll = corresp.locate(merloc);
     reflat = merll.lat;
     reflon = merll.lon;
 
      if (reflon < 0) reflon += 360.;
      // If there's a prior guess, use it.
      locip = merloc; locip += ip;
      locjp = merloc; locip += jp;
      if ( (merloc.i != corresp.xpoints()-1) && (itcount[locip] < itmax)) {
         tfl = corresp[locip];
         loc.i = tfl.i;
         loc.j = tfl.j;
      }
      else {
        if ( (merloc.j != corresp.ypoints()-1) && (itcount[locjp] < itmax)) {
           tfl = corresp[locjp];
           loc.i = tfl.i;
           loc.j = tfl.j;
        }
        else {
          loc.i = x.xpoints()/2;
          loc.j = x.ypoints()/2;
        }
      }

    tijloc.i = loc.i;
    tijloc.j = loc.j;
    fij = etaloc[loc];
    lloc.lat = fij.j; lloc.lon = fij.i;
    dlat = reflat - lloc.lat;
    dlon = reflon - lloc.lon;
    if (dlon < -360.) dlon += 360.;
    if (dlon > 360.) dlon -= 360.;

    while (iter < itmax && (fabs(dlat) > toler || fabs(dlon) > toler) ) {
      if (fabs(dlon) > fabs(dlat) )  {
        tijloc.i += dlon / dloc_di[loc].i;
      }
      else {
        tijloc.j += dlat / dloc_dj[loc].j;
      }
      loc.i = (int) (0.5 + tijloc.i);
      loc.j = (int) (0.5 + tijloc.j);

      fij = etaloc[loc];
      delta_ij = tijloc; delta_ij.i -= loc.i; delta_ij.j -= loc.j;
      fij +=  dloc_di[loc].i*delta_ij.i;
      fij +=  dloc_di[loc].i*delta_ij.i;
      fij +=  dloc_dj[loc].j*delta_ij.j;
      fij +=  dloc_dj[loc].j*delta_ij.j;
      lloc.lat = fij.j; lloc.lon = fij.i;
      dlat = reflat - lloc.lat;
      dlon = reflon - lloc.lon;
      if (dlon < -360.) dlon += 360.;
      if (dlon > 360.) dlon -= 360.;

      iter += 1;
    }
    corresp[merloc] = tijloc;
    itcount[merloc] = iter;

  }
  }

  return ;
}
