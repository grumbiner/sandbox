#include <cstdio>
#include <cstdlib>

#include "ncepgrids.h"
#include "points.h"

#define MAXLEN 900
class region {
  public:
    char name[MAXLEN];
    latpt ll, ur;
    region();
    region(char *, latpt &, latpt &);
    region(char *, double &, double &, double &, double &);
    region(double , double , double , double );
    bool in(latpt &);
};
region::region() {
  sprintf(name,"unknown\0");
  ll.lat = 0.0;
  ll.lon = 0.0;
  ur.lat = 90.0;
  ur.lon = 359.9;
}
region::region(double lat1, double lon1, double lat2, double lon2) {
  ll.lat = lat1;
  ur.lat = lat2;
  ll.lon = lon1;
  ur.lon = lon2;
}
region::region(char *nin, double &lat1, double &lon1, double &lat2, double &lon2) {
  strncpy(name, nin, MAXLEN);
  ll.lat = lat1;
  ur.lat = lat2;
  ll.lon = lon1;
  ur.lon = lon2;
}
region::region(char *nin, latpt &x, latpt &y) {
  strncpy(name, nin, MAXLEN);
  ll = x;
  ur = y;
}
bool region::in(latpt &x) {
  if (x.lat >= ll.lat && x.lat <= ur.lat &&
      x.lon >= ll.lon && x.lon <= ur.lon ) {
    return true;
  }
  else {
    return false;
  }
}
  

// Compute the net fluxes from the mrf in terms of an equivalent
//   melt/freeze rate when ice is present, and in terms of heating
//   a 25 meter ocean mixed layer when it isn't.
// Robert Grumbine 5 February 1998

// Restart 16 January 2009
//   assume that input files are set up,
//   read through, and average them up for whatever period is
//   provided

// 22 January 2009 
//   Read in 'accumulation' file and examine the net freeze/melt, and/or heating


void byregion(global_ice<float> sum, region &x) ;

int main(int argc, char *argv[]) {
// Inputs:
  gaussian<float> netmelt, netfreez, netheat, sum;

  ijpt xij;
  float count = 0.0, hrs = 3.0;
  float rho = 1000., cp = 4.e3, lf = 3.32e5, hmix = 25.0;
  FILE *metin, *fout;
  latpt ll;
  float hmin = 1.25, sstmin = 40.0;

// Working grid:
  global_ice<float> globe, flag;
  region gl(41.0, 264.0, 51.0, 285.0);
  region okhotsk( 40.0, 135.0, 65.0, 162.0);
  region caspian( 39.0, 33.0, 49.0, 55.0);
  region aa(-90.0, 0.0, -50, 360.0);

  metin = fopen(argv[1],"r");
  if (metin == (FILE *) NULL) {
    printf("failed to open %s\n",argv[1]);
    return 1;
  }
  //printf("grid size: %d by %d\n",lh.xpoints(), lh.ypoints() );
  netmelt.binin(metin);
  netfreez.binin(metin);
  netheat.binin(metin);
  fclose(metin);

  sum = netmelt;
  sum += netfreez; 
  sum *= -1; // melt corresponds to + flux from atm in prior convention

  float landval = -999, nonval = -999;
  flag.set((float) 0.0);
  globe.fromall(sum, landval, nonval); 

  for (xij.j = 0; xij.j < globe.ypoints() ; xij.j++) {
  for (xij.i = 0; xij.i < globe.xpoints() ; xij.i++) {
    if (globe[xij] != 0.0) flag[xij] = 1.0;
    //ll = sum.locate(xij);
    //ll.lat = -ll.lat;
    // flip lat as the gaussian grid of this vintage doesn't have
    // current (24 Jun 2009) sign convention
  }
  }
  printf("Global integral ice melt %f %f\n",globe.integrate(), globe.integrate()/flag.integrate() );

// Look at regions
  region nh(20.0, 0.0, 90.0, 360.0);
  region bering(50.0, 160.0, 66.7, 200.0);
  region hudson(50.0, 265.0, 70.0, 285.0);
  region baltic(52.0, 10.0, 67.0, 30.0);
  region azov(40.0, 30.0, 50.0, 40.0);
  region high_arctic(66.7, 0.0, 90.0, 360.0);

  printf("Great lakes  ");
  byregion(globe, gl);
  printf("Okhotsk  ");
  byregion(globe, okhotsk);
  printf("Bering  ");
  byregion(globe, bering);
  printf("Hudson  ");
  byregion(globe, hudson);
  printf("Baltic  ");
  byregion(globe, baltic);
  printf("Azov    ");
  byregion(globe, azov);
  printf("Caspian ");
  byregion(globe, caspian);
  printf("Arctic  ");
  byregion(globe, high_arctic);
   
  printf("\n");
  printf("Southern Hemisphere  ");
  byregion(globe, aa);
  printf("Northern Hemisphere  ");
  byregion(globe, nh);

  return 0;

}
void byregion(global_ice<float> globe, region &gl) {
  global_ice<float> flag;
  ijpt xij;
  latpt ll;

// Note that we are using a copy of the input 'globe', and not the original.
//  we are changing the values in that array.
  flag.set ((float) 0.0);
  for (xij.j = 0; xij.j < globe.ypoints() ; xij.j++) {
  for (xij.i = 0; xij.i < globe.xpoints() ; xij.i++) {
    ll = globe.locate(xij);
    ll.lat = -ll.lat;
    if (globe[xij] != 0.0 && gl.in(ll) ) {
       flag[xij] = 1.0;
    }
    else {
       globe[xij] = 0;
    }
  }
  }
  printf("integral ice melt %f average %f\n",globe.integrate(), globe.integrate()/flag.integrate() );

  return ;
}
