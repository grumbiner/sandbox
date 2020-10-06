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
  llgrid<float> salt(360, 12, 1.0, 1.0, -60, 0);
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
    saltin[loc] = tsalt;
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

  //tmpgrid->fromall(conc, nonval, flag);
  salt.fromall(*tmpgrid, nonval, flag);
  printf("salttest %f %f %f %f\n",salt.gridmax(), salt.gridmin(), 
      salt.average(), salt.rms() ); fflush(stdout);

  fout = fopen("salttest","w");
  salt.binout(fout);
  fclose(fout); 
// back from attempt (?)

  fout = fopen(argv[2], "w");
  salt.binout(fout);

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
