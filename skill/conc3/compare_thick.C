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
  cfs1deg<float>  cfs;      // cfs ice thickness
  gaussian<float> gfs;      // gfs ice thickness
  global_quarter<float> delta;


  ijpt loc, tloc;
  latpt ll;
  float undef = -1, toler;

  fin1 = fopen(argv[1], "r");
  fin2 = fopen(argv[2], "r");

  cfs.ftnin(fin1); 
  fclose(fin1);

  gfs.ftnin(fin2);
  fclose(fin2);

  toler = atof(argv[3]);

  undef = cfs.gridmax();
  printf("grid maxes gfs %f, cfs %f\n",gfs.gridmax(undef), cfs.gridmax(undef) );

  delta.set((float) 0.);

  for (loc.j = 0; loc.j < gfs.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < gfs.xpoints(); loc.i++) {
    ll = gfs.locate(loc);
    // manage gaussian flipping
    ll.lat = -ll.lat;
    tloc = cfs.locate(ll);
    if ( (fabs(gfs[loc] - cfs[tloc]) > toler) && (cfs[tloc] != undef) && (gfs[loc] != undef) ) {
      printf("%4d %4d  %8.5f %10.5f  %5.2f %5.2f  %6.2f\n",loc.i, loc.j, ll.lat, ll.lon,
        gfs[loc], cfs[tloc], gfs[loc] - cfs[tloc]); 
    }
  }
  }

// Compute volume changes, mean, absolute:
  double vol = 0, absvol = 0;
  double area = 0;

  for (loc.j = 0; loc.j < delta.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < delta.xpoints(); loc.i++) {
    ll = delta.locate(loc);
    tloc = cfs.locate(ll);
    if (cfs[tloc] != undef) { 
      delta[loc] = cfs[tloc];
      if (delta[loc] > 0) {
        area += delta.cellarea(loc);
      }

      ll.lat = -ll.lat;
      tloc = gfs.locate(ll);
      if (gfs[tloc] != undef) {
        delta[loc] -= gfs[tloc];
      }
    }

    if (delta[loc] != 0) {
      vol    += delta[loc]       * delta.cellarea(loc);
      absvol += fabs(delta[loc]) * delta.cellarea(loc);
    } 
  }
  }

  printf("[m/million km^2] delvol %f  absvol %f\n",vol/1.e12, absvol/1.e12);
  printf("Max difference = %f\n",delta.gridmax() );
  printf("cfs area in million km^2 %f\n",area/1.e12);

  return 0;
}
