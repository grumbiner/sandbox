#include "ncepgrids.h"


#define FLAG   -1
#define NFIELDS 3
#define MAXLAT 56.0
#define BUFFER  5
//  define resolution in degrees, will translate to km for polarstereos
#define DELTA   0.375


//////////////////////// Define lat-limited bedient-style grids (1/2 the range of full
///        bedient grids ///
template<class T>
class semi_bedient_north : public psgrid<T> {
  public:
    semi_bedient_north(int = 15 );
    semi_bedient_north(float = 15 );
};
template<class T>
semi_bedient_north<T>::semi_bedient_north(float fbedient) {
  #ifdef VERBOSE
    cout << "in ::semi_bedient_north(float)\n";
    cout.flush();
  #endif
  this->nx = (int) (0.5+32*fbedient);
  this->ny = (int) (0.5+32*fbedient);
  this->dx = 381.e3*32. / (float)this->nx;
  this->dy = 381.e3*32. / (float)this->ny;
  this->slat = 60.0;
  this->slon = -10.0;
  this->xorig = -(this->nx/2)*this->dx ;
  this->yorig = -(this->ny/2)*this->dy ;
  this->sgn   = 1.0;

// Calculate parameters here for later calculation (recalculate needed when
//   slat != 60.0
  double eccen2 = parameters::eccen2;
  double eccen  = sqrt(eccen2);
  this->sl = this->slat / parameters::degrees_per_radian;
  this->cm = cos(this->sl)/ sqrt(1.0-eccen2*sin(this->sl)*sin(this->sl) );
  this->tnaught  = tan(M_PI_4 - this->sl/2.) /
           pow(  ((1.0 - eccen*sin(this->sl))/(1.0+eccen*sin(this->sl))), eccen/2.);

  ijpt f;
  f.i = 0; f.j = 0;
  this->first_longitude = (this->locate(f)).lon;

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in semi_bedient_north(void)\n";
    cout.flush();}

  return ;
}
template<class T>
semi_bedient_north<T>::semi_bedient_north(int n) {
  #ifdef VERBOSE
    cout << "in ::semi_bedient_north(int)\n";
    cout.flush();
  #endif
  this->nx = 32*n;
  this->ny = 32*n;
  this->dx = 381.e3 / n;
  this->dy = 381.e3 / n;
  this->slat = 60.0;
  this->slon = -10.0;
  this->xorig = -(this->nx/2)*this->dx ;
  this->yorig = -(this->ny/2)*this->dy ;
  this->sgn   = 1.0;

// Calculate parameters here for later calculation (recalculate needed when
//   slat != 60.0
  double eccen2 = parameters::eccen2;
  double eccen  = sqrt(eccen2);
  this->sl = this->slat / parameters::degrees_per_radian;
  this->cm = cos(this->sl)/ sqrt(1.0-eccen2*sin(this->sl)*sin(this->sl) );
  this->tnaught  = tan(M_PI_4 - this->sl/2.) /
           pow(  ((1.0 - eccen*sin(this->sl))/(1.0+eccen*sin(this->sl))), eccen/2.);

  ijpt f;
  f.i = 0; f.j = 0;
  this->first_longitude = (this->locate(f)).lon;

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in semi_bedient_north(void)\n";
    cout.flush();}

  return ;
}
template<class T>
class semi_bedient_south : public psgrid<T> {
  public:
    semi_bedient_south(int = 15 );
};
template<class T>
semi_bedient_south<T>::semi_bedient_south(int n) {
  #ifdef VERBOSE
    cout << "in ::semi_bedient_south(int)\n";
    cout.flush();
  #endif
  this->nx = 32*n;
  this->ny = 32*n;
  this->dx = 381.e3 / n;
  this->dy = 381.e3 / n;
  this->slat =  60.0;
  this->slon = 170.0;
  this->xorig = -(this->nx/2)*this->dx ;
  this->yorig = -(this->ny/2)*this->dy ;
  this->sgn   = -1.0;

// Calculate parameters here for later calculation (recalculate needed when
//   slat != 60.0
  double eccen2 = parameters::eccen2;
  double eccen  = sqrt(eccen2);
  this->sl = this->slat / parameters::degrees_per_radian;
  this->cm = cos(this->sl)/ sqrt(1.0-eccen2*sin(this->sl)*sin(this->sl) );
  this->tnaught  = tan(M_PI_4 - this->sl/2.) /
           pow(  ((1.0 - eccen*sin(this->sl))/(1.0+eccen*sin(this->sl))), eccen/2.);

  ijpt f;
  f.i = 0; f.j = 0;
  this->first_longitude = (this->locate(f)).lon;

  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in semi_bedient_south(void)\n";
    cout.flush();}

  return ;
}
//////////////////////////////////////////////////////////////////////////




class rgglob : public metricgrid<float> {
#define NH 0
#define SH 2
#define LL 1
  private :
    llgrid<float> *latlon;
    psgrid<float> *nh;
    psgrid<float> *sh;

  public :
    metricgrid<float> *fields[NFIELDS];

  public : 
    rgglob(void);
// Dummy locate for now
    latpt locate(const fijpt &) { latpt y; y.lat = 0; y.lon = 0; return y;}

// Real fns:
    latpt  locate(const ijpt &);
    fijpt& locate(const latpt &);

    float& operator[](const ijpt &) ;
    float& operator[](const fijpt &) ;

    void operator=(const rgglob&);

    float gridmax();
    float gridmin();
    //void scale(float, float);

};
rgglob::rgglob(void) {
 // use constructors from the baser ps, ll classes
  int nbedient, isum = 0, np;
  float fbedient;
  fbedient = max(1.0, ( 381.0 / ((float)DELTA*111.1) ));
  nbedient = max(1, (int) (0.5 + fbedient) ) ;
  printf("approximate nbedient = %f %d\n",fbedient, nbedient);

  latlon = new llgrid<float>(360/DELTA, 2*(MAXLAT+BUFFER)/DELTA, 
                        DELTA, DELTA, -(MAXLAT+BUFFER), DELTA/2.0);
  nh = new semi_bedient_north<float> (fbedient);
  sh = new semi_bedient_south<float> (nbedient);

  fields[0] = nh;
  fields[1] = latlon;
  fields[2] = sh;

  for (int i = 0; i < NFIELDS; i++) {
    np = fields[i]->xpoints()*fields[i]->ypoints();
    printf("field %d has %d points\n",i,np);
    isum += np;
  }
  printf("%d total points\n",isum);

}
//void rgglob::scale(float bot, float top) {
//  ijpt loc;
//  for (loc.k = 0; loc.k < NFIELDS; loc.k++) {
//    for (loc.j = 0; loc.j < this->fields[loc.k]->ypoints(); loc.j++) {
//    for (loc.i = 0; loc.i < this->fields[loc.k]->xpoints(); loc.i++) {
//      if (this->operator[](loc) >= top) {this->operator[](loc) = 255; }
//      else {
//        this->operator[](loc) = 255*(top - this->operator[](loc))/(top-bot);
//      }
//    }
//    }
//  }
//}
   
latpt rgglob::locate(const ijpt &loc) {
  latpt ll;
  if (loc.k == NH) {
    ll = nh->locate(loc);
  }
  else if (loc.k == LL) {
    ll = latlon->locate(loc);
  }
  else if (loc.k == SH) {
    ll = sh->locate(loc);
  }
  return ll;
}

fijpt& rgglob::locate(const latpt &ll) {
  fijpt tmp;
  printf("ll.lat, maxlat %f %f\n", ll.lat, MAXLAT);

  if (ll.lat >= MAXLAT ) {
    printf("nh %f v %f\n", ll.lat, MAXLAT);
    global_fijpt.k = NH;
    tmp = nh->locate(ll);
    global_fijpt.i = tmp.i; global_fijpt.j = tmp.j;
  }
  else if (ll.lat > -MAXLAT ) {
    printf("ll %f v %f\n", ll.lat, -MAXLAT);
    global_fijpt.k = LL;
    tmp = latlon->locate(ll);
    global_fijpt.i = tmp.i; global_fijpt.j = tmp.j;
  }
  else {
    printf("sh %f v %f\n", ll.lat, -MAXLAT);
    global_fijpt.k = SH;
    tmp = sh->locate(ll);
    global_fijpt.i = tmp.i; global_fijpt.j = tmp.j;
  }

  return global_fijpt;
}
float& rgglob::operator[](const fijpt &loc) {
  switch ((int) loc.k) {
    case 0 : return( nh->operator[](loc)    ) ; break; 
    case 1 : return( latlon->operator[](loc) ) ; break;
    case 2 : return( sh->operator[](loc)    ) ; break;
    default : break;
  }
}
float& rgglob::operator[](const ijpt &loc) {
  switch (loc.k) {
    case 0 : return( nh->operator[](loc)     ) ; break; 
    case 1 : return( latlon->operator[](loc) ) ; break;
    case 2 : return( sh->operator[](loc)     ) ; break;
    default : break;
  }
}
void rgglob::operator=(const rgglob& x) {
  int k;
  for (k = 0; k < x.nh->xpoints()*x.nh->ypoints(); k++) {
    this->nh->operator[](k) = x.nh->operator[](k);
  }
  for (k = 0; k < x.sh->xpoints()*x.sh->ypoints(); k++) {
    this->sh->operator[](k) = x.sh->operator[](k);
  }
  for (k = 0; k < x.latlon->xpoints()*x.latlon->ypoints(); k++) {
    this->latlon->operator[](k) = x.latlon->operator[](k);
  }

  return;
}
float rgglob::gridmax(void) {
  float tmp = -FLT_MAX;
  for (int i = 0; i < NFIELDS; i++) {
    tmp = max(tmp, fields[i]->gridmax(FLAG) ); 
  }
  return (float) tmp;
}

float rgglob::gridmin(void) {
  float tmp = FLT_MAX;
  for (int i = 0; i < NFIELDS; i++) {
    tmp = min(tmp, fields[i]->gridmin(FLAG) ); 
  }
  return tmp;
}
