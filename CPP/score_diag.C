#include <stdio.h>
#include <stdlib.h>

#include "netcdf.h"
/* Handle errors by printing an error message and exiting with a
 * non-zero status. */
#define ERRCODE 2
#define ERR(e) {printf("Error: %s\n", nc_strerror(e)); exit(ERRCODE);}

#include "grid_math.h"
#include "ncepgrids.h"
void enter(grid2<float> &param, float *x) ;

#include "contingency_ptwise.C"

#define NX 4500
#define NY 3298

int main(int argc, char *argv[]) {
  float *x;
  int ncid, varid;
  int retval;
    ijpt loc;

// High res sea ice analysis from netcdf:
  global_12th<float> obs;
  global_12th<unsigned char> skip;
  FILE *fin;

// Hycom diag file variables of interest:
  grid2<float> lat(NX, NY), lon(NX, NY);
  grid2<float> ssh(NX, NY);
  grid2<float> ice_coverage(NX, NY), ice_thickness(NX, NY);
  grid2<float> u_barotropic_velocity(NX, NY), v_barotropic_velocity(NX, NY);

  x = (float*) malloc(sizeof(float)*NX*NY);

////////////////// skip grid ///////////////////////////////
  fin = fopen(argv[3], "r");
  skip.binin(fin);
  fclose(fin);
  #ifdef DEBUG
  printf("skip stats %d %d %d %d \n", skip.gridmax(), skip.gridmin(), skip.average(), skip.rms()); 
  #endif

////////////////// Sea ice analysis ///////////////////////////////
  fin = fopen(argv[2], "r");
  obs.ftnin(fin);
  fclose(fin);

  #ifdef DEBUG
  printf("obs stats %f %f %f %f \n", obs.gridmax(), obs.gridmin(), obs.average(), obs.rms()); 
  fflush(stdout);
  #endif

////////////////// Hycom variables ///////////////////////////////

  retval = nc_open(argv[1], NC_NOWRITE, &ncid);
  if (retval != 0) ERR(retval);

// go over all variables:
  retval = nc_inq_varid(ncid, "Latitude", &varid);
  if (retval != 0) ERR(retval);
  retval = nc_get_var_float(ncid, varid, x); 
  if (retval != 0) ERR(retval);fflush(stdout);
  enter(lat, x);

  retval = nc_inq_varid(ncid, "Longitude", &varid);
  if (retval != 0) ERR(retval);
  retval = nc_get_var_float(ncid, varid, x); 
  if (retval != 0) ERR(retval);fflush(stdout);
  enter(lon, x);


  retval = nc_inq_varid(ncid, "ice_coverage", &varid);
  if (retval != 0) ERR(retval);
  retval = nc_get_var_float(ncid, varid, x); 
  if (retval != 0) ERR(retval);fflush(stdout);
  enter(ice_coverage, x);
  #ifdef DEBUG
    palette<unsigned char> gg(19, 65);
    ice_coverage *= 100.;
    ice_coverage.xpm("ice.xpm",7,gg);
    loc.i = 0; loc.j = 0;
    printf("00 lat, ice: %f %f\n",ice_coverage[loc], lat[loc]);
    loc.i = lat.xpoints() - 1; loc.j = lat.ypoints() - 1;
    printf("NM lat, ice: %f %f\n",ice_coverage[loc], lat[loc]);
    return 0;
  #endif
  
  retval = nc_inq_varid(ncid, "ice_thickness", &varid);
  if (retval != 0) ERR(retval);

  retval = nc_get_var_float(ncid, varid, x); 
  if (retval != 0) ERR(retval);fflush(stdout);
  enter(ice_thickness, x);


  retval = nc_inq_varid(ncid, "u_barotropic_velocity", &varid);
  if (retval != 0) ERR(retval);
  retval = nc_get_var_float(ncid, varid, x); 
  if (retval != 0) ERR(retval);fflush(stdout);
  enter(u_barotropic_velocity, x);

  retval = nc_inq_varid(ncid, "v_barotropic_velocity", &varid);
  if (retval != 0) ERR(retval);
  retval = nc_get_var_float(ncid, varid, x); 
  if (retval != 0) ERR(retval);fflush(stdout);
  enter(v_barotropic_velocity, x);


  retval = nc_inq_varid(ncid, "ssh", &varid);
  if (retval != 0) ERR(retval);
  retval = nc_get_var_float(ncid, varid, x); 
  if (retval != 0) ERR(retval);fflush(stdout);
  enter(ssh, x);

// close when done:
  retval = nc_close(ncid);
  if (retval != 0) ERR(retval); fflush(stdout);

///////////////// End of Netcdf portion ////////////////////////////////////////////
//
// Now establish the matchup vectors
  int npts, count = 0;
  fijpt floc;
  latpt ll;
  
  npts = ice_coverage.xpoints()*ice_coverage.ypoints();
  mvector<float> observed(npts), model(npts);
  mvector<unsigned char> skipped(npts), north(npts), south(npts);

  for (loc.j = 0; loc.j < ice_coverage.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ice_coverage.xpoints(); loc.i++) {
    ll.lat = lat[loc];
    while (lon[loc] > 360.) lon[loc] -= 360.;
    ll.lon = lon[loc];
    
    floc = obs.locate(ll);

    #ifdef DEBUG
    if (floc.i <= -0.5 || floc.j <= -0.5) {
      printf("floc %f %f\n", floc.i, floc.j);
    }
    if (floc.i > obs.xpoints()-0.5 || floc.j > obs.ypoints()-0.5) {
      printf("floc %f %f\n", floc.i, floc.j);
    }
    #endif

    observed[count] = obs[floc];
    skipped[count]  = skip[floc];
    model[count]    = ice_coverage[loc];
    if ( ll.lat > 0 ) {
      north[count] = skipped[count];
      south[count] = 1; // 1 means do not use, 0 is to be used
    }
    else {
      north[count] = 1;
      south[count] = skipped[count];
    }

    #ifdef VERBOSE
    printf("%8d %5.3f %5.3f %3d  %7.3f %8.3f  %8.3f %8.3f\n",count,model[count], observed[count], skipped[count], lat[loc], lon[loc], floc.i, floc.j);
    #endif

    count++;
  }
  }


// At last, start scoring:
  float level;
  double a11, a12, a21, a22;
  float pod, far, fcr, pct, ts, bias;

  for (level = 0.0; level < 1.; level += 0.05) {
    contingency(observed, model, skipped, level, a11, a12, a21, a22);
    contingency_derived(a11, a12, a21, a22, pod, far, fcr, pct, ts, bias);
    printf("  level %4.2f  %f %f %f %f  %f %f %f %f %f %f\n",level, a11, a12, a21, a22, pod, far, fcr, pct, ts, bias);
  }
  for (level = 0.0; level < 1.; level += 0.05) {
    contingency(observed, model, north, level, a11, a12, a21, a22);
    contingency_derived(a11, a12, a21, a22, pod, far, fcr, pct, ts, bias);
    printf("nhlevel %4.2f  %f %f %f %f  %f %f %f %f %f %f\n",level, a11, a12, a21, a22, pod, far, fcr, pct, ts, bias);
  }
  for (level = 0.0; level < 1.; level += 0.05) {
    contingency(observed, model, south, level, a11, a12, a21, a22);
    contingency_derived(a11, a12, a21, a22, pod, far, fcr, pct, ts, bias);
    printf("shlevel %4.2f  %f %f %f %f  %f %f %f %f %f %f\n",level, a11, a12, a21, a22, pod, far, fcr, pct, ts, bias);
  }

  return 0;
}

void enter(grid2<float> &param, float *x) {
  ijpt loc;
  for (loc.j = 0; loc.j < param.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < param.xpoints(); loc.i++) {
    if (x[loc.i+ NX*loc.j] > 1e20) x[loc.i+ NX*loc.j] = 0;
    param[loc] = x[loc.i+ NX*loc.j];
  }
  }
  #ifdef DEBUG
  printf("stats: %f %f %f %f\n",param.gridmax(), param.gridmin(), param.average(), param.rms() );
  #endif

  return;
}

