#include "ncepgrids.h"

float increment(llgrid<float> &x, ijpt &loc) ;
float iterate(llgrid<unsigned char> &x, llgrid<float> &y, const unsigned char &flag) ;

int main(int argc, char *argv[]) {
  FILE *fin;

  global_12th<float> orig, old, tmp, y;
  global_12th<unsigned char> land;
  unsigned char landflag = 157;
  ijpt loc;
  float flag = -999., rms, limit = 1.e-9;
  int i;
  palette<unsigned char> gg(19, 65);

/////////////////////////////////
  fin = fopen("seaice_gland5min","r");
  land.binin(fin);
  fclose(fin);
  
  fin = fopen("sst","r");
  y.binin(fin);
  fclose(fin);
  y -= 271.35; // rtg's minimum temperature
  orig = y;
  old  = y;

  i = 0;
//  for (loc.j = 0; loc.j < y.ypoints(); loc.j++) {
//  for (loc.i = 0; loc.i < y.xpoints(); loc.i++) {
//    if (land[loc] == 157) {
//      y[loc] = flag;
//      i++;
//    }
//  }
//  }
//  //printf("%d land points\n", i); fflush(stdout);
/////////////////////////////////


  printf("y %f %f %f %f\n",y.gridmax(), y.gridmin(), y.average(), y.rms() );

  laplacean(y, tmp, flag);
  printf("%e %e %e %e\n",tmp.gridmax(), tmp.gridmin(), tmp.average(), tmp.rms() );
  //printf("%e %e %e %e\n",tmp.gridmax(flag), tmp.gridmin(flag), tmp.average(flag), tmp.rms(flag) );
  fflush(stdout);
  tmp.scale();
  tmp.xpm("start.xpm",7,gg);

  i = 0;
  rms = 9e9;
  char fname[900];
  FILE *fout, *delout;
  float dmax, dmin, drms, dave;

  fout = fopen("out","w");
  delout = fopen("delout","w");
  
  limit = 0.01;
  while (rms > limit) {
    //limit *= 1.414; 
    iterate(land, y, landflag);

    tmp = y;
    tmp -= old;
    //rms = tmp.rms();
    dmax = tmp.gridmax();
    dmin = tmp.gridmin();
    rms = max(dmax, -dmin);

    old = y;

    //drms = tmp.rms();
    //dave = tmp.average();
    //printf("%3d rms = %6.3e %6.3e %6.3f %7.3f limit %f\n",i,drms, dave, dmax, dmin, limit);
    //fflush(stdout);

    //if ((i < 10) || ( (i%5)== 0) ) {
    //  y.binout(fout);
    //  tmp.binout(delout);
    //}

    //sprintf(fname,"%03d.xpm",i);
    //tmp.scale();
    //tmp.xpm(fname,7,gg);

    i++;
  }
  y.binout(fout);

  tmp.binout(delout);
  orig -= y;
  orig.binout(delout); 

  fclose(fout);
  fclose(delout);
  

  laplacean(y, tmp, flag);
  printf("%e %e %e %e\n",tmp.gridmax(), tmp.gridmin(), tmp.average(), tmp.rms() );
  tmp.scale();
  tmp.xpm("finish.xpm",7,gg);

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

      //delta = del - x[loc];
      //sum  += delta;
      //sum2 += delta*delta;
      //lim = max(lim, abs(delta));
    }
  }
  }
  //printf("sum sum2 %e %e\n",sum, sum2); fflush(stdout);

  return lim;
}
