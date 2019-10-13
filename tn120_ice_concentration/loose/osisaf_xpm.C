#include "metric.h"

template<class T>
class north_osisaf : public psgrid<T> {
  public:
    north_osisaf(void);
    north_osisaf(north_osisaf<T> &);
    void operator=(grid2<T> &);
};
template<class T>
north_osisaf<T>::north_osisaf(void) {
  #ifdef VERBOSE
    cout << "in ::north_osisaf(void)\n";
  #endif
  this->nx = 760;
  this->ny = 1120;
  this->dx = 10.0e3;
  this->dy = 10.0e3;
  this->xorig = (-385. * this->dx );
  this->yorig = (-582. * this->dy );
  this->sgn = 1.0;
  this->slat = 60.0;
  this->slon = 45.0;
// rearth and eccen2 are taken from psgrid base class
// Calculate parameters here for later calculation (recalculate needed when
//   slat != 60.0
  double rearth = parameters::rearth;
  double eccen2 = parameters::eccen2;
  double eccen  = sqrt(eccen2);
  this->sl = this->slat / parameters::degrees_per_radian;
  this->cm = cos(this->sl)/ sqrt(1.0-eccen2*sin(this->sl)*sin(this->sl) );
  this->tnaught  = tan(M_PI_4 - this->sl/2.) /
           pow(  ((1.0 - eccen*sin(this->sl))/(1.0+eccen*sin(this->sl))), eccen/2.);

  this->grid = new T[this->nx*this->ny] ;
  if (this->grid == (T *) NULL) { cout << "Failed to new in north_osisaf(void)\n";
                            fflush(stdout); }
  this->pds.set_gridid(255);

  return ;
}
template<class T>
north_osisaf<T>::north_osisaf(north_osisaf<T> &x) {
  #ifdef VERBOSE
    cout << "Constructing a north_osisaf\n"; cout.flush();
  #endif
  if (this->grid != (T *) NULL ) {
    cout << "In north_osisaf constructor, needing to delete the grid *\n";
    cout.flush();
    delete []this->grid;
  }
  this->nx = x.xpoints();
  this->ny = x.ypoints();
  this->dx = x.dx;
  this->dy = x.dy;
  this->xorig = x.xorig;
  this->yorig = x.yorig;
  this->sgn = x.sgn;
  this->slat = x.slat;
  this->slon = x.slon;
// rearth and eccen2 are taken from psgrid base class

  this->grid = new T[this->nx*this->ny] ;
  if (this->grid == (T *) NULL) { cout << "Failed to new in north_osisaf(void)\n";
                            fflush(stdout); }
  this->pds.set_gridid(255);

  return ;
}
template<class T>
void north_osisaf<T>::operator=(grid2<T> &x) {
  int j;
  if (x.xpoints() != this->nx || x.ypoints() != this->ny) {
    cout << "size mismatch\n";
  }
  for (j = 0; j < this->ny*this->nx; j++) {
      this->grid[j] = x[j];
  }
  this->pds.set_gridid(255);
}

int main(int argc, char *argv[]) {
  grid2<float> nh(760,1120), sh(790, 830);
  north_osisaf<float> osi;
  palette<unsigned char> gg(19, 65);
  FILE *fin;
  ijpt loc;
  latpt ll;

  fin = fopen(argv[1],"r");
  nh.binin(fin);
  fclose(fin);
  fin = fopen(argv[2],"r");
  sh.binin(fin);
  fclose(fin);

  osi = nh;
  loc.j = 0; loc.i = 0; ll = osi.locate(loc);
  printf("00 is at %f %f\n",ll.lat, ll.lon);

  nh.scale();
  nh.xpm("nh.xpm",7,gg);
  sh.scale();
  sh.xpm("sh.xpm",7,gg);

  return 0;
}
