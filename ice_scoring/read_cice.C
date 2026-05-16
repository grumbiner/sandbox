#include <stdio.h>
#include <stdlib.h>

// Robert Grumbine


#include "netcdf.h"
/* Handle errors by printing an error message and exiting with a
 * non-zero status. */
#define ERRCODE 2
#define ERR(e) {printf("Error: %s\n", nc_strerror(e)); exit(ERRCODE);}

#include "grid_math.h"
void enter(grid2<float> &param, float *x) ;
bool bounded(grid2<float> &x, grid2<float> &lat, grid2<float> &lon, float bottom, float top) ;

#define NX 4500
#define NY 3297
// note that for cice, NY = 1 less than for hycom

int main(int argc, char *argv[]) {
  float *x;
  int ncid, varid;
  int retval;

//all:
  grid2<float> lat(NX, NY), lon(NX, NY);
  grid2<float> ulat(NX, NY), ulon(NX, NY);

//cice_inst:
  grid2<float> hi(NX, NY), hs(NX, NY), tsfc(NX, NY), aice(NX, NY);
  grid2<float> sst(NX, NY), sss(NX, NY);
  grid2<float> uvel(NX, NY), vvel(NX, NY);

  x = (float*) malloc(sizeof(float)*NX*NY);

/////////////////////////////////////////////////////////////

  retval = nc_open(argv[1], NC_NOWRITE, &ncid); if (retval != 0) ERR(retval);

  //could also get ULON, ULAT
  retval = nc_inq_varid(ncid, "TLAT", &varid); if (retval != 0) ERR(retval); 
  retval = nc_get_var_float(ncid, varid, x); if (retval != 0) ERR(retval);
  enter(lat, x);

  retval = nc_inq_varid(ncid, "TLON", &varid); if (retval != 0) ERR(retval);
  retval = nc_get_var_float(ncid, varid, x);if (retval != 0) ERR(retval); 
  enter(lon, x);
  // could fold lons here

// go over all physical variables:
  retval = nc_inq_varid(ncid, "aice", &varid); if (retval != 0) ERR(retval);
  retval = nc_get_var_float(ncid, varid, x); if (retval != 0) ERR(retval); 
  enter(aice, x);

  retval = nc_inq_varid(ncid, "hi", &varid); if (retval != 0) ERR(retval);
  retval = nc_get_var_float(ncid, varid, x); if (retval != 0) ERR(retval); 
  enter(hi, x);

  retval = nc_inq_varid(ncid, "uvel", &varid); if (retval != 0) ERR(retval);
  retval = nc_get_var_float(ncid, varid, x); if (retval != 0) ERR(retval); 
  enter(uvel, x);
  printf("uvel check\n");
  bounded(uvel, lat, lon, -0.5, 0.5);

  retval = nc_inq_varid(ncid, "vvel", &varid); if (retval != 0) ERR(retval);
  retval = nc_get_var_float(ncid, varid, x); if (retval != 0) ERR(retval);
  enter(vvel, x);
  printf("vvel check\n");
  bounded(vvel, lat, lon, -0.5, 0.5);

  retval = nc_inq_varid(ncid, "sst", &varid); if (retval != 0) ERR(retval);
  retval = nc_get_var_float(ncid, varid, x); if (retval != 0) ERR(retval); 
  enter(sst, x);
  printf("sst check\n");
  bounded(sst, lat, lon, -1.98, 35.0);

  retval = nc_inq_varid(ncid, "sss", &varid); if (retval != 0) ERR(retval);
  retval = nc_get_var_float(ncid, varid, x); if (retval != 0) ERR(retval); 
  enter(sss, x);
  printf("sss check\n");
  bounded(sss, lat, lon, 0, 41.0);

// close when done:
  retval = nc_close(ncid); if (retval != 0) ERR(retval);

// additional checks -- print out other vars if u or v out of bounds:
  ijpt loc;
  for (loc.j = 0; loc.j < aice.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < aice.xpoints(); loc.i++) {
    if (fabs(uvel[loc]) > 0.5 || fabs(vvel[loc]) > 0.5) {
      printf("all %f %f  %f %f %f  %f %f\n",lat[loc], lon[loc], uvel[loc], vvel[loc], 
                sqrt(uvel[loc]*uvel[loc]+vvel[loc]*vvel[loc]), aice[loc], hi[loc]);
    }
  }
  }
  


  return 0;
}

void enter(grid2<float> &param, float *x) {
  ijpt loc;
  for (loc.j = 0; loc.j < param.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < param.xpoints(); loc.i++) {
    if (x[loc.i+ NX*loc.j] > 1e30) x[loc.i+ NX*loc.j] = 0;
    param[loc] = x[loc.i+ NX*loc.j];
  }
  }
  printf("stats: %f %f %f %f\n",param.gridmax(), param.gridmin(), param.average(), param.rms() );

  return;
}
bool bounded(grid2<float> &x, grid2<float> &lat, grid2<float> &lon, float bottom, float top) {
  bool oob = false;
  ijpt loc;
  for (loc.j = 0; loc.j < lat.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < lat.xpoints(); loc.i++) {
    if (x[loc] > top) {
      oob = true;
      printf("%f %f  %f > %f\n",lat[loc], lon[loc], x[loc], top);
    }
    else if (x[loc] < bottom) {
      oob = true;
      printf("%f %f  %f < %f\n",lat[loc], lon[loc], x[loc], bottom);
    }
  }
  }

  return oob;
}
