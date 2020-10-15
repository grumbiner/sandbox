#include "ncepgrids.h"

template<class T>
class cfs1deg : public llgrid<T> {
  public:
   cfs1deg(void); /* Construction creator */
   ~cfs1deg(void) { };
   cfs1deg(cfs1deg<T>&);
   cfs1deg<T> & operator=(cfs1deg<T> &);
};
template<class T> 
cfs1deg<T> & cfs1deg<T>::operator=(cfs1deg<T> &x) {
  int loc;
  this->nx = 360;
  this->ny = 181;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in cfs1deg()\n"; }
  this->dlat = 1.0;
  this->dlon = 1.0;
  this->firstlat = -90.0;
  this->firstlon =  0.0;
  this->pds.set_gridid(3);
//Future: copy the pds
  for (loc = 0; loc < this->ny*this->nx; loc++) {
     this->grid[loc] = x[loc];
  }
  return *this;
}
template<class T>
cfs1deg<T>::cfs1deg(cfs1deg<T> &x) {
  int loc;
  this->nx = 360;
  this->ny = 181;
  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in cfs1deg()\n"; }
  this->dlat = 1.0;
  this->dlon = 1.0;
  this->firstlat = -90.0;
  this->firstlon =  0.0;
  this->pds.set_gridid(3);
//Future: copy the pds
  for (loc = 0; loc < this->ny*this->nx; loc++) {
     this->grid[loc] = x[loc];
  }
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;

}
template<class T>
cfs1deg<T>::cfs1deg(void) {
  this->nx = 360 ;
  this->ny = 181;
  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in cfs1deg()\n"; }
  this->dlat = 1.0;
  this->dlon = 1.0;
  this->firstlat = -90.0;
  this->firstlon =  0.0;
  this->pds.set_gridid(3);
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
}


int main(int argc, char *argv[]) {
  FILE *fin1, *fin2, *fout;
  global_12th<float> analy, delta; // reference is sea ice analysis on native grid
  //cfs1deg<float> fcst;      // start with fcst = cfs 1 degree output
  gaussian<float> fcst;      // see if default t574 behaves

//// Lower resolution intercomparisons -- quarter degree:
//  grid2<float> tlow(analy.xpoints()/3, analy.ypoints()/3) ;
//  global_quarter<float> low, delta;
// Lower resolution intercomparisons -- one degree:
//  grid2<float> tlow(analy.xpoints()/12, analy.ypoints()/12) ;
//  global_quarter<float> low, delta;

  ijpt loc, tloc;
  latpt ll;
  float fcst_undef = -1, toler;

  fin1 = fopen(argv[1], "r");
  fin2 = fopen(argv[2], "r");

  analy.binin(fin1);
  //analy.ftnin(fin1); // for grib2 analyses
  fclose(fin1);

  fcst.ftnin(fin2); // ftnin since grib2 binary output seems to force ftn
  fclose(fin2);

  toler = atof(argv[3]);

  if (fcst.gridmax() > 300) {
    fcst_undef = fcst.gridmax();
  }
  if (fcst.gridmax(fcst_undef) < 3.) fcst *= 100.;
  if (analy.gridmax(fcst_undef) < 3.)          analy *= 100.;
  if (fcst.gridmax() > 300) {
    fcst_undef = fcst.gridmax();
  }

  printf("grid maxes analy %f, fcst %f\n",analy.gridmax(), fcst.gridmax(fcst_undef) );

//  tlow.reduce(analy);
//  printf("reduced grid max, min, avg: %f %f %f\n",tlow.gridmax(), tlow.gridmin(), tlow.average() );
//  for (loc.j = 0; loc.j < low.ypoints(); loc.j++) {
//  for (loc.i = 0; loc.i < low.xpoints(); loc.i++) {
//    low[loc] = tlow[loc];
//  }
//  }

// separately, check undefs against sea ice land mask -- should be def on all nonland
  delta.set((float) 0.);
// swap out original 'analy' for 'low' -- quarter degree for round1
  for (loc.j = 0; loc.j < analy.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < analy.xpoints(); loc.i++) {
    ll = analy.locate(loc);
    // manage gaussian flipping
    ll.lat = -ll.lat;
    tloc = fcst.locate(ll);
    if ( (fabs(analy[loc] - fcst[tloc]) > toler) && (fcst[tloc] != fcst_undef) ) {
      ll.lat = -ll.lat; // restore from gaussian for output
      printf("%4d %4d  %9.6f %11.6f  %6.1f %6.1f  %6.1f\n",loc.i, loc.j, ll.lat, ll.lon,
        analy[loc], fcst[tloc], analy[loc] - fcst[tloc]); 
      delta[loc] =  analy[loc] - fcst[tloc];
    }
  }
  }

// Compute areal changes, mean, absolute:
  double area = 0, absarea = 0;

  for (loc.j = 0; loc.j < analy.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < analy.xpoints(); loc.i++) {
    if (delta[loc] != 0) {
      area += delta[loc]/100*delta.cellarea(loc);
      absarea += fabs(delta[loc])/100 * delta.cellarea(loc);
    } 
  }
  }
  printf("delarea %f  absarea %f\n",area/1.e9, absarea/1.e9);
  

  return 0;
}
