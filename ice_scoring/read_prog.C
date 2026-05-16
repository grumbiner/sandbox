#include <stdio.h>
#include <stdlib.h>

//Robert Grumbine

#include "netcdf.h"
/* Handle errors by printing an error message and exiting with a
 * non-zero status. */
#define ERRCODE 2
#define ERR(e) {printf("Error: %s\n", nc_strerror(e)); exit(ERRCODE);}

#include "grid_math.h"
void enter(grid2<float> &param, float *x) ;
bool bounded(grid2<float> &x, grid2<float> &lat, grid2<float> &lon, float bottom, float top) ;

#define NX 4500
#define NY 3298

int main(int argc, char *argv[]) {
  float *x;
  int ncid, varid;
  int retval;

//all:
  grid2<float> lat(NX, NY), lon(NX, NY);

//prog:
  grid2<float> sst(NX, NY), sss(NX, NY);
  grid2<float> u_velocity(NX, NY), v_velocity(NX, NY);

//diag  grid2<float> ssh(NX, NY);
//diag  grid2<float> ice_coverage(NX, NY), ice_thickness(NX, NY);
//diag  grid2<float> u_barotropic_velocity(NX, NY), v_barotropic_velocity(NX, NY);


  x = (float*) malloc(sizeof(float)*NX*NY);

/////////////////////////////////////////////////////////////

  //retval = nc_open("nctest", NC_NOWRITE, &ncid);
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
  // could fold lons here


//diag  retval = nc_inq_varid(ncid, "ice_coverage", &varid);
//diag  printf("retval inq = %d\n",retval); if (retval != 0) ERR(retval);
//diag
//diag  retval = nc_get_var_float(ncid, varid, x); 
//diag  printf("retval get = %d\n",retval); if (retval != 0) ERR(retval);fflush(stdout);
//diag  enter(ice_coverage, x);
//diag  palette<unsigned char> gg(19, 65);
//diag  ice_coverage *= 100.;
//diag  ice_coverage.xpm("ice.xpm",7,gg);
//diag
//diag  retval = nc_inq_varid(ncid, "ice_thickness", &varid);
//diag  printf("retval inq = %d\n",retval); if (retval != 0) ERR(retval);
//diag
//diag  retval = nc_get_var_float(ncid, varid, x); 
//diag  printf("retval get = %d\n",retval); if (retval != 0) ERR(retval);fflush(stdout);
//diag  enter(ice_thickness, x);

//diag  retval = nc_inq_varid(ncid, "u_barotropic_velocity", &varid);
//diag  printf("retval inq = %d\n",retval); if (retval != 0) ERR(retval);
//diag
//diag  retval = nc_get_var_float(ncid, varid, x); 
//diag  printf("retval get = %d\n",retval); if (retval != 0) ERR(retval);fflush(stdout);
//diag  enter(u_barotropic_velocity, x);
//diag
//diag  retval = nc_inq_varid(ncid, "v_barotropic_velocity", &varid);
//diag  printf("retval inq = %d\n",retval); if (retval != 0) ERR(retval);
//diag
//diag  retval = nc_get_var_float(ncid, varid, x); 
//diag  printf("retval get = %d\n",retval); if (retval != 0) ERR(retval);fflush(stdout);
//diag  enter(v_barotropic_velocity, x);

  retval = nc_inq_varid(ncid, "u_velocity", &varid);
  printf("retval inq = %d\n",retval); if (retval != 0) ERR(retval);

  retval = nc_get_var_float(ncid, varid, x); 
  printf("retval get = %d\n",retval); if (retval != 0) ERR(retval);fflush(stdout);
  enter(u_velocity, x);

  retval = nc_inq_varid(ncid, "v_velocity", &varid);
  printf("retval inq = %d\n",retval); if (retval != 0) ERR(retval);

  retval = nc_get_var_float(ncid, varid, x); 
  printf("retval get = %d\n",retval); if (retval != 0) ERR(retval);fflush(stdout);
  enter(v_velocity, x);


//diag  retval = nc_inq_varid(ncid, "ssh", &varid);
//diag  printf("retval inq = %d\n",retval); if (retval != 0) ERR(retval);
//diag
//diag  retval = nc_get_var_float(ncid, varid, x); 
//diag  printf("retval get = %d\n",retval); if (retval != 0) ERR(retval);fflush(stdout);
//diag  enter(ssh, x);

  retval = nc_inq_varid(ncid, "sst", &varid);
  printf("retval inq = %d\n",retval); if (retval != 0) ERR(retval);

  retval = nc_get_var_float(ncid, varid, x); 
  printf("retval get = %d\n",retval); if (retval != 0) ERR(retval);fflush(stdout);
  enter(sst, x);
  printf("sst check\n");
  bounded(sst, lat, lon, -1.98, 35.0);

  retval = nc_inq_varid(ncid, "sss", &varid);
  printf("retval inq = %d\n",retval); if (retval != 0) ERR(retval);

  retval = nc_get_var_float(ncid, varid, x); 
  printf("retval get = %d\n",retval); if (retval != 0) ERR(retval);fflush(stdout);
  enter(sss, x);
  printf("sss check\n");
  bounded(sss, lat, lon, 0, 41.0);

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
