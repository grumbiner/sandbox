#include "ncepgrids.h"

// class flake -- = llgrid<float>
class flake : public llgrid<float> {
  public:
     flake(void);
};
flake::flake(void) {
  this->nx = 1740;
  this->ny =  840;
  this->dlat = 1./30.;
  this->dlon = 1./30.;
  this->firstlon = -124.896;
  this->firstlat =  24.976;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;


  this->grid = new float [this->nx*this->ny];

  if (this->grid == (float* ) NULL) {cout << "failed to new a flake\n"; }

  return;
}


int main(int argc, char *argv[]) {
  FILE *flakein, *rtgin, *landin, *distin;
  global_12th<float> rtg, dist;
  global_12th<unsigned char> land;
  flake climo, rtgout;
  float toler = 0.01, flake_flag;

  ijpt loc, loc2;
  latpt ll;

  toler = atof(argv[1]);
  flakein = fopen(argv[2], "r");
  rtgin   = fopen(argv[3], "r");
  landin  = fopen(argv[4], "r");
  distin  = fopen(argv[5], "r");

  land.binin(landin);
  fclose(landin);
  dist.binin(distin);
  fclose(distin);

  rtg.binin(rtgin);
  rtgout.set(-9.0);
  fclose(rtgin);
  if (rtg.gridmax() > 273.) rtg -= 273.15;
 
  climo.binin(flakein);
  fclose(flakein);
  flake_flag = climo.gridmax();
  printf("climo max min average %f %f %f\n",climo.gridmax(flake_flag), 
            climo.gridmin(flake_flag), climo.average(flake_flag) );

  if (climo.gridmax(flake_flag) > 273.) climo -= 273.15;
  flake_flag = climo.gridmax();
  printf("climo max min average %f %f %f\n",climo.gridmax(flake_flag), 
            climo.gridmin(flake_flag), climo.average(flake_flag) );

  for (loc.j = 0; loc.j < climo.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < climo.xpoints(); loc.i++) {
    if (climo[loc] == flake_flag) climo[loc] = -9.;

    ll = climo.locate(loc);
    loc2 = rtg.locate(ll);
    if ( (land[loc2] == 0) && climo[loc] != -9. ) {
      rtgout[loc] = rtg[loc2];
      if ( (fabs(climo[loc] - rtg[loc2]) > toler) ) {

        printf("%7.3f %8.3f  %6.2f %6.2f  %6.2f  %6.2f\n", ll.lat, ll.lon, rtg[loc2], 
                climo[loc], rtg[loc2] - climo[loc], dist[loc2]/1000. );
      }
      climo[loc] -= rtg[loc2];
    }
    else {
      climo[loc] = -9.;
    }
    
  }
  }

  FILE *fout;
  fout = fopen("delout","w");
  climo.binout(fout);
  rtgout.binout(fout);
  fclose(fout);

  return 0;
}
