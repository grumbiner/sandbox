#include <stdio.h>
#include <stdlib.h>

#include "netcdf.h"
/* Handle errors by printing an error message and exiting with a
 * non-zero status. */
#define ERRCODE 2
#define ERR(e) {printf("Error: %s\n", nc_strerror(e)); exit(ERRCODE);}

//#include "grid_math.h"
#include "ncepgrids.h"
#include "resops.h"
void enter(grid2<float> &param, float *x) ;

#define NX 4500
#define NY 3298

int main(int argc, char *argv[]) {
  grid2<float> lat(NX, NY), lon(NX, NY);
  grid2<float> ssh(NX, NY);
  grid2<float> ice_coverage(NX, NY), ice_thickness(NX, NY);
  grid2<float> u_barotropic_velocity(NX, NY), v_barotropic_velocity(NX, NY);

  FILE *fin;
  global_12th<float> obs;
  fin = fopen(argv[2], "r");
  obs.binin(fin);
  fclose(fin);
  printf("obs stats %f %f %f %f\n",obs.gridmax(), obs.gridmin(), obs.average(), obs.rms() );

/////////////////////////////////////////////////////////////
  float *x;
  int ncid, varid;
  int retval;
  x = (float*) malloc(sizeof(float)*NX*NY);

  retval = nc_open(argv[1], NC_NOWRITE, &ncid);
  printf("retval open = %d\n",retval); fflush(stdout);

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
//  palette<unsigned char> gg(19, 65);
//  ice_coverage *= 100.;
//  ice_coverage.xpm("ice.xpm",7,gg);

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


//  retval = nc_inq_varid(ncid, "ssh", &varid);
//  printf("retval inq = %d\n",retval); if (retval != 0) ERR(retval);
//
//  retval = nc_get_var_float(ncid, varid, x); 
//  printf("retval get = %d\n",retval); if (retval != 0) ERR(retval);fflush(stdout);
//  enter(ssh, x);

// close when done:
  retval = nc_close(ncid);
  printf("retval close = %d\n",retval);if (retval != 0) ERR(retval); fflush(stdout);

// end netcdf //////////////////////////////////////////
////////////////////////////////////////////////////////

// try construction of a readin(resops) grid:
  readin<float> *gridtest;
  gridtest = new readin<float>(lat, lon);
  ijpt loc;
  for (loc.j = 0; loc.j < lat.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < lat.xpoints(); loc.i++) {
    printf("%4d %4d %e %e\n",loc.i, loc.j, gridtest->jacobian(loc), gridtest->cellarea(loc) );
  }
  }
  return 0;
 
  


////////////////////////////////////////////////////////
// Start scoring:
  latpt ll;
  fijpt floc;
  //ijpt loc;
  FILE *fout;
  fout = fopen(argv[3],"w");

  for (loc.j = 0; loc.j < ice_coverage.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ice_coverage.xpoints(); loc.i++) {
    ll.lat = lat[loc];
    while (lon[loc] > 360.) lon[loc] -= 360.;
    ll.lon = lon[loc] ;
    //if (ll.lon > 360.) {
    //  printf("still out of range %f\n",ll.lon);
    //}

    floc = obs.locate(ll);
    if (floc.i <= -0.5 || floc.j <= -0.5) {
      printf("floc %f %f\n", floc.i, floc.j);
    }
    if (floc.i > obs.xpoints()-0.5 || floc.j > obs.ypoints()-0.5) {
      printf("floc %f %f\n", floc.i, floc.j);
    }

    if (fabs(lat[loc]) > 30.0 && (obs[floc] > 0 && ice_coverage[loc] > 0) ) {
      fprintf(fout, "%4d %4d  %8.4f %8.4f  %5.3f %5.3f  %6.3f\n",
         loc.i, loc.j, lat[loc], lon[loc], ice_coverage[loc], obs[floc], 
         obs[floc] - ice_coverage[loc]);
    }
  }
    fflush(stdout);
  }
  fclose(fout);
  


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

