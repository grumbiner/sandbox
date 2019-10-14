#include "ncepgrids.h"

template<class T>
class nic : public psgrid<T> {
  public:
    nic(void);
};
template<class T>
nic<T>::nic(void) {
  #ifdef VERBOSE
    cout << "in ::nic(void)\n";
    cout.flush();
  #endif
  this->nx = 6144;
  this->ny = 6144;
  this->dx = 4000.;
  this->dy = 4000.;
  this->xorig = (-this->nx/2* this->dx );
  this->yorig = (-this->ny/2* this->dy );
  this->sgn = 1.0;
  this->slat = 60.0;
  this->slon = (-00.0);
// rearth and eccen2 are taken from psgrid base class

   this->grid = new T[this->nx*this->ny] ;
   if (this->grid == (T *) NULL) { cout << "Failed to new in nic(void)\n";
     cout.flush(); }
   this->pds.set_gridid(255);

  return ;
}

int main(void) {
  FILE *fin;
  nic<float> conc;
  psgrid<float> thick(6144,6144, 3077,3077, 60.0, 0., 1., 4000., 4000.);
  int i;
  float flag = -1.;
  palette<unsigned char> gg(19, 65);
  mvector<int> histogram(2000);

  fin = fopen("fred","r");
  conc.binin(fin);
  thick.binin(fin);
  fclose(fin);

  for (i = 0; i < conc.xpoints() * conc.ypoints() ; i++) {
    if (conc[i] > 120) conc[i] = -1;
    if (thick[i] > 12000) thick[i] = -1;
  }

  printf("conc %f %f %f %f\n",conc.gridmax(flag), conc.gridmin(flag), conc.average(flag), conc.rms(flag) );
  printf("thick %f %f %f %f\n",thick.gridmax(flag), thick.gridmin(flag), thick.average(flag), thick.rms(flag) );

  histogram = 0;
  for (i = 0; i < conc.xpoints() * conc.ypoints() ; i++) {
    if (conc[i] >= 0 && conc[i] < histogram.xpoints()) histogram[conc[i]] += 1;
  }
  printf("concentration\n");
  for (i = 0; i < histogram.xpoints(); i++) {
    if (histogram[i] != 0) printf("%4d %d\n",i,histogram[i]);
  }


  histogram = 0;
  for (i = 0; i < conc.xpoints() * conc.ypoints() ; i++) {
    if (thick[i] >= 0 && thick[i] < histogram.xpoints()) histogram[thick[i]] += 1;
  }
  printf("thickness\n");
  for (i = 0; i < histogram.xpoints(); i++) {
    if (histogram[i] != 0) printf("%4d %d\n",i,histogram[i]);
  }

////  conc.scale();
  conc.xpm("conc.xpm",7,gg);
  thick.scale();
  thick.xpm("thick.xpm",7,gg);

  ijpt loc;
  latpt ll;
  loc.i = 0; loc.j = 0;
  ll = conc.locate(loc);
  printf("%4d %4d  %f %f\n",loc.i, loc.j, ll.lat, ll.lon); 
  loc.i = 3077; loc.j = 0;
  ll = conc.locate(loc);
  printf("%4d %4d  %f %f\n",loc.i, loc.j, ll.lat, ll.lon); 
  loc.i = 3077; loc.j = 3077;
  ll = conc.locate(loc);
  printf("%4d %4d  %f %f\n",loc.i, loc.j, ll.lat, ll.lon); 
  
  return 0;
}
