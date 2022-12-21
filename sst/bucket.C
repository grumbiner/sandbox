#include "ncepgrids.h"

///////////////////////////////////////////////////////////////////////////////////////////////////

void  reflag(llgrid<float> &x) ;
void  check(llgrid<float> &x) ;
float fold_lon(float lon) ;


///////////////////////////////////////////////////////////////////////////////////////////////////
#include "resops.h"

int main(int argc, char *argv[]) { 
  FILE *fin, *fout;
  readin<float> *tmpgrid;
  //llgrid<float> salt(360, 12, 1.0, 1.0, -60, 0);
  //llgrid<int> count(360, 12, 1.0, 1.0, -60, 0);
  global_12th<float> salt;
  global_12th<int> count;
  ijpt loc, tloc;
  latpt ll;
  char fname[900];
  float nonval = -1., flag = -1.;
  grid2<float> lat(4500,3298), lon(4500,3298), saltin(4500,3298);

  float tlon, tlat, tsalt;
  fin = fopen(argv[1],"r");
  for (loc.j = 0; loc.j < saltin.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < saltin.xpoints(); loc.i++) {
    fscanf(fin, "%f %f %f\n",&tlon, &tlat, &tsalt);
    lat[loc] = tlat;
    lon[loc] = tlon;
    if (tsalt >= 0) {
      saltin[loc] = tsalt;
    }
    else {
      saltin[loc] = nonval;
    }
  }
  }
  fclose(fin);

// Now try the resop version of the grid:
  tmpgrid = new readin<float> (lat, lon);
  //printf("past trying to new a 'readin'\n"); fflush(stdout);

  printf("nx ny = %d %d\n",tmpgrid->xpoints(), tmpgrid->ypoints() );
  for (loc.j = 0; loc.j < tmpgrid->ypoints(); loc.j++) { 
  for (loc.i = 0; loc.i < tmpgrid->xpoints(); loc.i++) {
    tmpgrid->operator[](loc) = saltin[loc];
  }
  }

  printf("tmpgrid %f %f %f %f\n",tmpgrid->gridmax(nonval), 
     tmpgrid->gridmin(nonval), tmpgrid->average(nonval), tmpgrid->rms(nonval) ); fflush(stdout);

  
  count.set(0);
  salt.set((float) -1);
  for (loc.j = 0; loc.j < tmpgrid->ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < tmpgrid->xpoints(); loc.i++) {
    ll = tmpgrid->locate(loc);
    if (ll.lon > 360) ll.lon -= 360.;
    tloc = salt.locate(ll);
    if (salt.in(tloc) ) {
      salt[tloc] += tmpgrid->operator[](loc);
      count[tloc] += 1;
    }
  }
  } 

  global_12th<float> mask;
  mask.set((float) 0);
  for (loc.j = 0; loc.j < salt.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < salt.xpoints(); loc.i++) {
    if (count[loc] != 0) salt[loc] /= count[loc];
    if (salt[loc] == -1) {
      ll = salt.locate(loc);
      mask[loc] = nonval;
      printf("did not fill %d %d  %f %f\n",loc.i, loc.j, ll.lon, ll.lat);
    }
  }
  }

  global_12th<float> salt2;
  salt2.fromall(salt, mask, nonval, flag);
  printf("salt1 %f %f %f %f\n",salt.gridmax(), salt.gridmin(), 
      salt.average(), salt.rms() ); fflush(stdout);
  printf("salt2 %f %f %f %f\n",salt2.gridmax(), salt2.gridmin(), 
      salt2.average(), salt2.rms() ); fflush(stdout);

  fout = fopen("salttest","w");
  salt2.binout(fout);
  fclose(fout); 
// back from attempt (?)

  fout = fopen(argv[2], "w");
  salt2.binout(fout);

  lat.binout(fout);
  lon.binout(fout);

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
