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

#define NX 4500
#define NY 3298

int main(int argc, char *argv[]) {
  float *x;
  int ncid, varid;
  int retval;

  grid2<float> lat(NX, NY), lon(NX, NY);
// diag:
  grid2<float> ssh(NX, NY);
  grid2<float> ice_coverage(NX, NY), ice_thickness(NX, NY);
  grid2<float> u_barotropic_velocity(NX, NY), v_barotropic_velocity(NX, NY);
//prog:
  //grid2<float> sst(NX, NY), sss(NX, NY);
  //grid2<float> u_velocity(NX, NY), v_velocity(NX, NY);


  x = (float*) malloc(sizeof(float)*NX*NY);

/////////////////////////////////////////////////////////////

  retval = nc_open(argv[1], NC_NOWRITE, &ncid);
  printf("retval open = %d\n",retval); fflush(stdout);
  if (retval != 0) ERR(retval);

// go over all variables:
  retval = nc_inq_varid(ncid, "Latitude", &varid);
  printf("retval inq = %d\n",retval); if (retval != 0) ERR(retval);

  retval = nc_get_var_float(ncid, varid, x); 
  printf("retval get = %d\n",retval); if (retval != 0) ERR(retval);fflush(stdout);
  enter(lat, x);

  retval = nc_inq_varid(ncid, "Longitude", &varid);
  printf("retval inq = %d\n",retval); if (retval != 0) ERR(retval);

  retval = nc_get_var_float(ncid, varid, x); 
  printf("retval get = %d\n",retval); if (retval != 0) ERR(retval);fflush(stdout);
  enter(lon, x);


  retval = nc_inq_varid(ncid, "ice_coverage", &varid);
  printf("retval inq = %d\n",retval); if (retval != 0) ERR(retval);

  retval = nc_get_var_float(ncid, varid, x); 
  printf("retval get = %d\n",retval); if (retval != 0) ERR(retval);fflush(stdout);
  enter(ice_coverage, x);
  palette<unsigned char> gg(19, 65);
  ice_coverage *= 100.;
  ice_coverage.xpm("ice.xpm",7,gg);
  ijpt loc;
  loc.i = 0; loc.j = 0;
  printf("00 lat, ice: %f %f\n",ice_coverage[loc], lat[loc]);
  loc.i = lat.xpoints() - 1; loc.j = lat.ypoints() - 1;
  printf("NM lat, ice: %f %f\n",ice_coverage[loc], lat[loc]);
  
  retval = nc_inq_varid(ncid, "ice_thickness", &varid);
  printf("retval inq = %d\n",retval); if (retval != 0) ERR(retval);

  retval = nc_get_var_float(ncid, varid, x); 
  printf("retval get = %d\n",retval); if (retval != 0) ERR(retval);fflush(stdout);
  enter(ice_thickness, x);


  retval = nc_inq_varid(ncid, "u_barotropic_velocity", &varid);
  printf("retval inq = %d\n",retval); if (retval != 0) ERR(retval);

  retval = nc_get_var_float(ncid, varid, x); 
  printf("retval get = %d\n",retval); if (retval != 0) ERR(retval);fflush(stdout);
  enter(u_barotropic_velocity, x);

  retval = nc_inq_varid(ncid, "v_barotropic_velocity", &varid);
  printf("retval inq = %d\n",retval); if (retval != 0) ERR(retval);

  retval = nc_get_var_float(ncid, varid, x); 
  printf("retval get = %d\n",retval); if (retval != 0) ERR(retval);fflush(stdout);
  enter(v_barotropic_velocity, x);


  retval = nc_inq_varid(ncid, "ssh", &varid);
  printf("retval inq = %d\n",retval); if (retval != 0) ERR(retval);

  retval = nc_get_var_float(ncid, varid, x); 
  printf("retval get = %d\n",retval); if (retval != 0) ERR(retval);fflush(stdout);
  enter(ssh, x);

//  retval = nc_inq_varid(ncid, "sst", &varid);
//  printf("retval inq = %d\n",retval); if (retval != 0) ERR(retval);
//
//  retval = nc_get_var_float(ncid, varid, x); 
//  printf("retval get = %d\n",retval); if (retval != 0) ERR(retval);fflush(stdout);
//  enter(sst, x);

// close when done:
  retval = nc_close(ncid);
  printf("retval close = %d\n",retval);if (retval != 0) ERR(retval); fflush(stdout);


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

