#include "ncepgrids.h"

template<class T>
class rtofsg : public llgrid<T> {
  public :
    rtofsg(void);
    rtofsg(rtofsg<T> &);
  // now start unique:

};
template <class T>
rtofsg<T>::rtofsg(void) {
  this->nx = 4500;
  this->ny = 3298;
  this->dlat = 0.04;
  this->dlon = 0.08;
  this->firstlat =  40.0;
  this->firstlon = -180.0;
  this->grid = new T [this->nx * this->ny];

  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
  this->pds.set_gridid(255);

}
template <class T>
rtofsg<T>::rtofsg(rtofsg<T> &x) {
  this->nx = x.nx;
  this->ny = x.ny;
  this->dlat = x.dlat;
  this->dlon = x.dlon;
  this->firstlat = x.firstlat;
  this->firstlon = x.firstlon;

  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
  this->pds.set_gridid(255);

  if (this->grid != (T *) NULL) {
    delete this->grid;
  }
  this->grid = new T[this->nx*this->ny];
  for (int i = 0; i < this->ny*this->nx; i++) {
    this->grid[i] = x[i];
  }
}
///////////////////////////////////////////////////////////////////////////////////////////////////

void transfer(rtofsg<float> &aice, rtofsg<float> &lat, rtofsg<float> &lon, 
              global_12th<int> &count, global_12th<float> &conc);
void  reflag(llgrid<float> &x) ;
void  check(llgrid<float> &x) ;
float fold_lon(float lon) ;


///////////////////////////////////////////////////////////////////////////////////////////////////
#include "resops.h"

int main(int argc, char *argv[]) { 
  FILE *fin, *fout;
  rtofsg<float> aice, hi, ssh, hml, lat, lon;
  readin<float> *tmpgrid;
  global_12th<float> conc, thick, eta, ml;
  global_12th<int> count;
  ijpt loc, tloc;
  latpt ll;
  char fname[900];
  float nonval = -99., flag = -99.;

  fin = fopen(argv[1],"r");
  aice.ftnin(fin);
  hi.ftnin(fin);
  ssh.ftnin(fin);
  hml.ftnin(fin);
  lat.ftnin(fin);
  lon.ftnin(fin);
  fclose(fin);

  printf(" aice "); reflag(aice);
  printf(" hi   "); reflag(hi);
  printf(" ssh  "); reflag(ssh);
  printf(" hml  "); reflag(hml);
  printf(" lat  "); check(lat);
  printf(" lon  "); check(lon);

// Now try the resop version of the grid:
  tmpgrid = new readin<float> (lat, lon);
  printf("past trying to new a 'readin'\n"); fflush(stdout);
  printf("nx ny = %d %d\n",tmpgrid->xpoints(), tmpgrid->ypoints() );
  for (loc.j = 0; loc.j < tmpgrid->ypoints(); loc.j++) { 
  for (loc.i = 0; loc.i < tmpgrid->xpoints(); loc.i++) {
    tmpgrid->operator[](loc) = aice[loc];
  }
  }
  printf("tmpgrid %f %f %f %f\n",tmpgrid->gridmax(nonval), 
     tmpgrid->gridmin(nonval), tmpgrid->average(nonval), tmpgrid->rms(nonval) ); fflush(stdout);
  //tmpgrid->fromall(conc, nonval, flag);
  conc.fromall(*tmpgrid, nonval, flag);
  printf("conctest %f %f %f %f\n",conc.gridmax(), conc.gridmin(), 
      conc.average(), conc.rms() ); fflush(stdout);
  fout = fopen("conctest","w");
  conc.binout(fout);
  fclose(fout); 
// back from attempt (?)

  transfer(aice, lat, lon, count, conc);
  transfer(hi,   lat, lon, count, thick);
  transfer(ssh,  lat, lon, count, eta);
  transfer(hml,  lat, lon, count, ml);
  printf(" conc  "); check(conc);
  printf(" thick "); check(thick);
  printf(" eta   "); check(eta);
  printf("  ml   "); check(ml);

  fout = fopen(argv[2], "w");
  conc.binout(fout);
  thick.binout(fout);
  eta.binout(fout);
  ml.binout(fout);

  lat.binout(fout);
  lon.binout(fout);
  aice.binout(fout);
  hi.binout(fout);
  ssh.binout(fout);
  hml.binout(fout);

  fclose(fout);

  return 0;
}

void  reflag(llgrid<float> &x) {
  float nonval;
  float universal = -99.;
  nonval = x.gridmax();
  printf(" %f %f %f %f \n",x.gridmax(nonval), x.gridmin(nonval), 
                           x.average(nonval), x.rms(nonval) ); 
// reflag nonvals to consistent number:
  for (int i = 0; i < x.xpoints()*x.ypoints(); i++) {
    if (x[i] == nonval) x[i] = universal;
  }

  return;
}
void  check(llgrid<float> &x) {
  printf(" %f %f %f %f \n",x.gridmax(), x.gridmin(), 
                           x.average(), x.rms() ); 
  return;
}
void transfer(rtofsg<float> &aice, rtofsg<float> &lat, rtofsg<float> &lon, 
              global_12th<int> &count, global_12th<float> &conc) {
  ijpt loc, tloc;
  latpt ll;

  count.set(0);
  conc.set((float) -99.0);
//
  for (loc.j = 0; loc.j < lat.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < lat.xpoints(); loc.i++) {
    if (aice[loc] != -99.0) {
      ll.lat = lat[loc];
      ll.lon = fold_lon(lon[loc]);
      tloc = count.locate(ll);
      if (conc[tloc] == -99.0) {
        conc[tloc]  = aice[loc];
        count[tloc] = 1;
      }
      else {
        conc[tloc]  += aice[loc];
        count[tloc] += 1;
      }
    }
  }
  }
//
  for (loc.j = 0; loc.j < count.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < count.xpoints(); loc.i++) {
    if (count[loc] != 0) conc[loc] /= (float) count[loc]; 
  }
  }
  printf("count max min %d %d\n",count.gridmax(), count.gridmin() );

  return;
}
float fold_lon(float lon) {
  if (fabs(lon) < 360.) {
    return lon;
  }
  else {
    while (lon > 360.) { lon -= 360.; }
    while (lon < -360.) { lon += 360.; }
    return lon;
  }
}
