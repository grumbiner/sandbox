#include "metric.h"

template<class T>
class north_nesdis_4km : public psgrid<T> {
  public:
    north_nesdis_4km(void); /*Construction */
    double show_slat();
};
template<class T> 
north_nesdis_4km<T>::north_nesdis_4km(void)
{
  int bedient = 96;
  this->nx = 64*bedient ;
  this->ny = 64*bedient ;
  this->dx = 381.e3 / bedient; /*This defines it as a 1/8th Bedient grid */
  this->dy = 381.e3 / bedient;
  this->slat = 58.3625;
  this->slon = -10.0;
  this->sgn   = 1.0;
  this->xorig = -(this->nx/2 - 0.5)*this->dx;
  this->yorig = -(this->ny/2 - 0.5)*this->dy;

  this->rearth = parameters::rearth;
  this->eccen2 = parameters::eccen2;
  double eccen  = sqrt(this->eccen2);
  this->sl = this->slat / parameters::degrees_per_radian;
  this->cm = cos(this->sl)/ sqrt(1.0-this->eccen2*sin(this->sl)*sin(this->sl) );
  this->tnaught  = tan(M_PI_4 - this->sl/2.) /
           pow(  ((1.0 - eccen*sin(this->sl))/(1.0+eccen*sin(this->sl))), eccen/2.);

  this->grid = new T[this->nx*this->ny]; 
  if (this->grid == (T *) NULL) { cout << "Failed to new in north_nesdis_4km(void)\n";}
 
  return ; 
}
template<class T>
double north_nesdis_4km<T>::show_slat(void) {
  printf("slat = %f\n",this->slat); fflush(stdout);
  printf("tnaught = %f\n",this->tnaught); fflush(stdout);
  return this->slat;
}

int main(void) {
  north_nesdis_4km<unsigned char> x;
  ijpt loc;
  fijpt floc, fdelta;
  latpt ll, refll;
  FILE *finlat, *finlon;
  double meand = 0., meandi = 0., meandj = 0.;
  int count = 0;

  //x.show_slat();
  //return 0;
  finlat = fopen("latitude.dat","r");
  finlon = fopen("longitude.dat","r");
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    ll = x.locate(loc);
    if (ll.lon < 0.) ll.lon += 360.;
    fscanf(finlat,"%f\n",&refll.lat);
    fscanf(finlon,"%f\n",&refll.lon);
    floc = x.locate(refll);
    if (floc.i != -2.) {
      meandi += floc.i - loc.i;
      meandj += floc.j - loc.j;
      meand  += sqrt(fdelta.i*fdelta.i+fdelta.j*fdelta.j);
      fdelta = floc;
      fdelta.i -= loc.i;
      fdelta.j -= loc.j;
      meand  += sqrt(fdelta.i*fdelta.i+fdelta.j*fdelta.j);
      count += 1;
      printf("%4d %4d  %f %f  floc %10.6f %10.6f  %9.6f\n",loc.i, loc.j, 
      //printf("%4d %4d  %f %f  del %f %f  floc %f %f  %f\n",loc.i, loc.j, 
        //ll.lat, ll.lon, refll.lat - ll.lat, refll.lon - ll.lon, 
        ll.lat, ll.lon, 
        fdelta.i, fdelta.j, sqrt(fdelta.i*fdelta.i+fdelta.j*fdelta.j) );
    }
  }
  }
  printf("mean_err %f  meandi = %f, meandj = %f\n",
             meand/count, meandi/count, meandj/count);

  return 0;
}
