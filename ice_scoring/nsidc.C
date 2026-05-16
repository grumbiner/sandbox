#include <stdio.h>
#include <stdlib.h>

// shell for nsdc grid experiments

#include "netcdf.h"
/* Handle errors by printing an error message and exiting with a
 * non-zero status. */
#define ERRCODE 2
#define ERR(e) {printf("Error: %s\n", nc_strerror(e)); exit(ERRCODE);}

#include "grid_math.h"
#include "ncepgrids.h"
template <class T>
void enter(grid2<float> &param, T *x) ;
template <class T>
void rev_enter(grid2<float> &param, T *x) ;

int main(int argc, char *argv[]) {
  float *xf;
  unsigned char *xb;
  double *xd;

  int ncid, varid;
  int retval;

// High res sea ice analysis from netcdf:
  nsidcnorth<float> obs;
  grid2<float> lat(obs.ypoints(), obs.xpoints()), lon(obs.ypoints(), obs.xpoints());
  grid2<float> tmp(obs.ypoints(), obs.xpoints());

  xf = (float*) malloc(sizeof(float)*obs.xpoints()*obs.ypoints() );
  xb = (unsigned char*) malloc(sizeof(unsigned char)*obs.xpoints()*obs.ypoints() );
  xd = (double*) malloc(sizeof(double)*obs.xpoints()*obs.ypoints() );

////////////////// Sea ice analysis ///////////////////////////////
  retval = nc_open(argv[1], NC_NOWRITE, &ncid);
  if (retval != 0) ERR(retval);

  retval = nc_inq_varid(ncid, "latitude", &varid);
  if (retval != 0) ERR(retval);
  retval = nc_get_var_double(ncid, varid, xd); 
  if (retval != 0) ERR(retval);fflush(stdout);
  enter(lat, xd);

  retval = nc_inq_varid(ncid, "longitude", &varid);
  if (retval != 0) ERR(retval);
  retval = nc_get_var_double(ncid, varid, xd); 
  if (retval != 0) ERR(retval);fflush(stdout);
  enter(lon, xd);

  retval = nc_inq_varid(ncid, "seaice_conc_cdr", &varid);
  if (retval != 0) ERR(retval);
  retval = nc_get_var_uchar(ncid, varid, xb); 
  if (retval != 0) ERR(retval);fflush(stdout);
  enter(tmp, xb);

// close when done:
  retval = nc_close(ncid);
  if (retval != 0) ERR(retval); fflush(stdout);

////////////////// Latlon check ///////////////////////////////
  obs.set((float) 157.0);
  fijpt floc;
  latpt ll;
  for (int i = 0; i < tmp.xpoints()*tmp.ypoints(); i++) {
    ll.lat = lat[i];
    ll.lon = lon[i];
    floc = obs.locate(ll);
    obs[floc] = tmp[i];
    printf("%6d %.3f %.3f  %3.0f  %.3f %.3f  %f %f\n",i,lat[i], lon[i], tmp[i], floc.i, floc.j, fabs(floc.i - rint(floc.i)) , fabs(floc.j - rint(floc.j) ) );
    
  }
  for (int i = 0; i < tmp.xpoints()*tmp.ypoints(); i++) {
    if (obs[i] == 157.0) {
      printf("failed to update %d\n",i);
    }
  }

  printf("obs stats %f %f %f %f \n", obs.gridmax(), obs.gridmin(), obs.average(), obs.rms()); 
  printf("tmp stats %f %f %f %f \n", tmp.gridmax(), tmp.gridmin(), tmp.average(), tmp.rms()); 
  fflush(stdout);

  return 0;

///////////////// End of Netcdf portion ////////////////////////////////////////////
  
  return 0;
}

template <class T>
void enter(grid2<float> &param, T *x) {
  ijpt loc;
  for (loc.j = 0; loc.j < param.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < param.xpoints(); loc.i++) {
    if (x[loc.i+ param.xpoints()*loc.j] > 1e20) x[loc.i+ param.xpoints()*loc.j] = 0;
    param[loc] = x[loc.i+ param.xpoints()*loc.j];
  }
  }
  #ifdef DEBUG
  printf("stats: %f %f %f %f\n",param.gridmax(), param.gridmin(), param.average(), param.rms() );
  #endif

  return;
}
template <class T>
void rev_enter(grid2<float> &param, T *x) {
  ijpt loc;
  int index = 0;
  for (loc.j = param.ypoints()-1; loc.j >= 0; loc.j--) {
  for (loc.i = 0; loc.i < param.xpoints(); loc.i++) {
    //index = (param.ypoints() - loc.j) + param.ypoints()*loc.i;
    if (x[index] > 1e20) x[index] = 0;
    param[loc] = (float) x[index];
    index++;
  }
  }

  printf("stats: %f %f %f %f\n",param.gridmax(), param.gridmin(), param.average(), param.rms() );
  return;
}
