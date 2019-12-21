#include <stdio.h>
#include <stdlib.h>

#include "netcdf.h"
/* Handle errors by printing an error message and exiting with a
 * non-zero status. */
#define ERRCODE 2
#define ERR(e) {printf("Error: %s\n", nc_strerror(e)); exit(ERRCODE);}

#include "ncepgrids.h"
void enter(grid2<float> &param, float *x) ;
void get_nc(char *fname, grid2<float> &lat, grid2<float> &lon, grid2<float> &aice, grid2<float> &hi);


void  scoring(FILE *fout, grid2<float> &aice, grid2<float> &lat, grid2<float> &lon, global_12th<float> &obs) ;
void  scoring(FILE *fout, grid2<float> &aice, grid2<float> &lat, grid2<float> &lon, global_12th<float> &obs, global_12th<unsigned char> &skip, float level) ;

#define NX 4500
#define NY 3297

int main(int argc, char *argv[]) {
  FILE *fin;

  grid2<float> lat(NX, NY), lon(NX, NY);
  grid2<float> aice(NX, NY), hi(NX, NY);

  global_12th<float> obs;
  global_12th<unsigned char> skip;

// Get observations and the global skip mask:
  fin = fopen(argv[1], "r");
  obs.binin(fin);
  fclose(fin);
  //printf("obs stats %f %f %f %f\n",obs.gridmax(), obs.gridmin(), obs.average(), obs.rms() );

  // get skip mask
  fin = fopen(argv[2], "r");
  skip.binin(fin);
  fclose(fin);
  //printf("skip stats %d %d %d %d\n",skip.gridmax(), skip.gridmin(), skip.average(), skip.rms() );


///////// NETCDF gets ///////////////////////////////////////////
  get_nc(argv[3], lat, lon, aice, hi);

////////////////////////////////////////////////////////
// Start scoring:
  FILE *fout;
  global_12th<unsigned char> nh, sh;
  ijpt loc;
  latpt ll;

  fout = fopen(argv[4],"w");

  fprintf(fout, "global stats\n");
  scoring(fout, aice, lat, lon, obs);
  scoring(fout, aice, lat, lon, obs, skip, 0.0);
  scoring(fout, aice, lat, lon, obs, skip, 0.05);
  scoring(fout, aice, lat, lon, obs, skip, 0.15);
  scoring(fout, aice, lat, lon, obs, skip, 0.50);
  scoring(fout, aice, lat, lon, obs, skip, 0.70);
  scoring(fout, aice, lat, lon, obs, skip, 0.88);
  scoring(fout, aice, lat, lon, obs, skip, 0.94);
  nh = skip;
  sh = skip;
  #ifdef DEBUG
  int n = 0, s = 0;
  for (loc.j = 0; loc.j < nh.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < nh.xpoints(); loc.i++) {
    ll = nh.locate(loc);
    if (ll.lat > 0) {
      sh[loc] = 1;
      s++;
    }
    else {
      nh[loc] = 1;
      n++;
    }
  }
  }
  printf("n,s = %d %d\n",n,s);
  #endif

  fprintf(fout, "nh stats\n");
  scoring(fout, aice, lat, lon, obs, nh, 0.0);
  scoring(fout, aice, lat, lon, obs, nh, 0.05);
  scoring(fout, aice, lat, lon, obs, nh, 0.15);
  scoring(fout, aice, lat, lon, obs, nh, 0.50);
  scoring(fout, aice, lat, lon, obs, nh, 0.70);
  scoring(fout, aice, lat, lon, obs, nh, 0.88);
  scoring(fout, aice, lat, lon, obs, nh, 0.94);

  fprintf(fout, "sh stats\n");
  scoring(fout, aice, lat, lon, obs, sh, 0.0);
  scoring(fout, aice, lat, lon, obs, sh, 0.05);
  scoring(fout, aice, lat, lon, obs, sh, 0.15);
  scoring(fout, aice, lat, lon, obs, sh, 0.50);
  scoring(fout, aice, lat, lon, obs, sh, 0.70);
  scoring(fout, aice, lat, lon, obs, sh, 0.88);
  scoring(fout, aice, lat, lon, obs, sh, 0.94);

  fclose(fout);

  return 0;
}

void  scoring(FILE *fout, grid2<float> &aice, grid2<float> &lat, grid2<float> &lon, global_12th<float> &obs, global_12th<unsigned char> &skip, float level) {
  latpt ll;
  fijpt floc;
  ijpt loc;
  double sum = 0, sumsq = 0;
  int count = 0;
  int a11 = 0, a12 = 0, a21 = 0, a22 = 0;

// aice, lat, lon, obs, fout
  for (loc.j = 0; loc.j < aice.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < aice.xpoints(); loc.i++) {
    ll.lat = lat[loc];
    while (lon[loc] > 360.) lon[loc] -= 360.;
    ll.lon = lon[loc] ;

    floc = obs.locate(ll);
    #ifdef DEBUG
      if (floc.i <= -0.5 || floc.j <= -0.5) {
        fprintf(fout, "floc %f %f\n", floc.i, floc.j);
      }
      if (floc.i > obs.xpoints()-0.5 || floc.j > obs.ypoints()-0.5) {
        fprintf(fout, "floc %f %f\n", floc.i, floc.j);
      }
    #endif

    // only score if point is water of interest, and at least one of observed or fcst is >= level
    if ( (skip[floc] == 0) ) {
  
      sum += obs[floc] - aice[loc];
      sumsq += (obs[floc] - aice[loc])*(obs[floc] - aice[loc]);
      count += 1;

     // contingency table:
     if (aice[loc] > level ) {
       if (obs[floc] > level ) {
         a11 += 1;
       }
       else {
         a12 += 1;
       }
     }
     else {
       if (obs[floc] > level ) {
         a21 += 1;
       }
       else {
         a22 += 1;
       }
     }
    }

  }
  }
  //printf("count = %d %d\n",count, a11+a12+a21+a22);

  fprintf(fout, "skip mean, rms = %6.3f %6.3f level = %5.2f\n",sum/count, sqrt(sumsq/count), level );
  fprintf(fout, "skip contingency %6.3f %6.3f %6.3f %6.3f  %5.2f\n",(float)a11/(float)count, (float)a12/(float)count,
        (float)a21/(float)count, (float)a22/(float)count, level);
  double pod, far, fcr, pct, ts, bias;
  pod = (double) a11 / (double) (a11 + a12);
  far = (double) a12 / (double) (a12 + a11);
  fcr = (double) a21 / (double) (a21 + a22);
  pct = ((double) a11 + (double) a22) / ( (double) a11+a12+a21+a22); //percent correct
  ts  = (double) a11 / ( (double) a11+a12+a21); // threat score, aka csi - critical success index
  bias = (double) (a11+a12) / ((double)(a11+a21));
  fprintf(fout, "skip pod etc %6.3f %6.3f %6.3f %6.3f %6.3f %f   %5.2f\n",pod, far, fcr, pct, ts, bias, level);


  return;
}
void  scoring(FILE *fout, grid2<float> &aice, grid2<float> &lat, grid2<float> &lon, global_12th<float> &obs) {
  latpt ll;
  fijpt floc;
  ijpt loc;
  double sum = 0, sumsq = 0;

// aice, lat, lon, obs, fout
  for (loc.j = 0; loc.j < aice.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < aice.xpoints(); loc.i++) {
    ll.lat = lat[loc];
    while (lon[loc] > 360.) lon[loc] -= 360.;
    ll.lon = lon[loc] ;

    floc = obs.locate(ll);
    if (floc.i <= -0.5 || floc.j <= -0.5) {
      fprintf(fout, "floc %f %f\n", floc.i, floc.j);
    }
    if (floc.i > obs.xpoints()-0.5 || floc.j > obs.ypoints()-0.5) {
      fprintf(fout, "floc %f %f\n", floc.i, floc.j);
    }

    sum   += (obs[floc] - aice[loc]);
    sumsq += (obs[floc] - aice[loc])*(obs[floc] - aice[loc]);

  }
    fflush(fout);
  }
  fprintf(fout, "global mean, rms = %e %e\n",sum/aice.xpoints()/aice.ypoints(), sqrt(sumsq/aice.xpoints()/aice.ypoints()) );

  return;
}

void  get_nc(char *fname, grid2<float> &lat, grid2<float> &lon, grid2<float> &aice, grid2<float> &hi) {
  float *x;
  int ncid, varid;
  int retval;
  x = (float*) malloc(sizeof(float)*lat.xpoints()*lat.ypoints());

//tlat, tlon, aice, hi
  retval = nc_open(fname, NC_NOWRITE, &ncid); if (retval != 0) ERR(retval);

// go over all variables:
  retval = nc_inq_varid(ncid, "TLAT", &varid); if (retval != 0) ERR(retval);
  retval = nc_get_var_float(ncid, varid, x); if (retval != 0) ERR(retval); 
  enter(lat, x);

  retval = nc_inq_varid(ncid, "TLON", &varid); if (retval != 0) ERR(retval);
  retval = nc_get_var_float(ncid, varid, x); if (retval != 0) ERR(retval); 
  enter(lon, x);

  retval = nc_inq_varid(ncid, "aice", &varid); if (retval != 0) ERR(retval);
  retval = nc_get_var_float(ncid, varid, x); if (retval != 0) ERR(retval); 
  enter(aice, x);

  retval = nc_inq_varid(ncid, "hi", &varid); if (retval != 0) ERR(retval);
  retval = nc_get_var_float(ncid, varid, x); if (retval != 0) ERR(retval); 
  enter(hi, x);

// close when done:
  retval = nc_close(ncid); if (retval != 0) ERR(retval);

  return;
}
void enter(grid2<float> &param, float *x) {
  ijpt loc;
  for (loc.j = 0; loc.j < param.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < param.xpoints(); loc.i++) {
    if (x[loc.i+ NX*loc.j] > 1e30) x[loc.i+ NX*loc.j] = 0;
    param[loc] = x[loc.i+ NX*loc.j];
  }
  }
  //printf("stats: %f %f %f %f\n",param.gridmax(), param.gridmin(), param.average(), param.rms() );

  return;
}
