#include "ncepgrids.h"

float iterate(llgrid<unsigned char> &x, llgrid<float> &y, const unsigned char &flag) ;

int main(int argc, char *argv[]) {
  FILE *fin, *fout, *delout;
  global_12th<float> old, tmp, y;
  global_12th<unsigned char> land;
  unsigned char landflag = 157;
  float flag = -999., rms, limit = 1.e-2, dmax, dmin;
  int i;

/////////////////////////////////
  fin = fopen("seaice_gland5min","r");
  land.binin(fin);
  fclose(fin);
  
  fin = fopen("sst","r");
  y.binin(fin);
  fclose(fin);
  y -= 271.35; // rtg's minimum temperature
  old  = y;

  i = 0;
/////////////////////////////////

  printf("y %f %f %f %f\n",y.gridmax(), y.gridmin(), y.average(), y.rms() );
  laplacean(y, tmp, flag);
  printf("%e %e %e %e\n",tmp.gridmax(), tmp.gridmin(), tmp.average(), tmp.rms() );
  fflush(stdout);

  i = 0;
  rms = 9e9;

  while (rms > limit) {
    
    iterate(land, y, landflag);
    tmp = y;
    tmp -= old;

    //rms = tmp.rms();
    dmax = tmp.gridmax();
    dmin = tmp.gridmin();
    rms = max(dmax, -dmin);

    old = y;
    i++;
  }

  fin = fopen("sst","r");
  old.binin(fin);
  fclose(fin);
  old -= 271.35;
  old -= y;
  fout   = fopen("out","w");
  delout = fopen("delout","w");

  y += 271.35; // restore rtg's shift
  y.binout(fout);

  old.binout(delout); 

  fclose(fout);
  fclose(delout);

  return 0;
}
float iterate(llgrid<unsigned char> &land, llgrid<float> &y, const unsigned char &flag) {
  ijpt loc;
  float lim = 0;
  double del;
  double sum = 0.0, sum2 = 0.0;
  double c1, c2, c3;
  ijpt ip, jp, im, jm;
  latpt ll;
  double dlat, dlon;
  double theta, divisor;


// precompute some variables
  dlat = y.dlat;
  dlon = y.dlon;
  dlat *= M_PI / 180.;
  dlon *= M_PI / 180.;

  c1 = 1./dlat/dlat;

  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
    ip.j = loc.j;
    jp.j = loc.j+1;
    im.j = loc.j;
    jm.j = loc.j-1;
    ll = y.locate(loc); 
    theta = ll.lat * M_PI/180.;
    divisor = 2./cos(theta)/cos(theta)/dlon/dlon + 2./dlat/dlat;
    c2 = 1./dlon/dlon/cos(theta)/cos(theta);
    c3 = tan(theta)/2./dlat;

  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
    if (land[loc] == flag) { 
      jm.i = loc.i;    
      jp.i = loc.i;    
      ip.i = loc.i + 1; 
      im.i = loc.i - 1;

      del  = (y[jp] + y[jm])*c1 + (y[ip] + y[im])*c2;
      del -= c3*(y[jp]-y[jm]);
      y[loc] = del/divisor; 

    }
  }
  }

  return lim;
}
