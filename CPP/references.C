#include <ctype.h>
#include <stdlib.h>
#include "ncepgrids.h"
#ifdef W3LIB
  #include <time.h>
#endif

// Program to construct NCEP-ized climatology from Chapman-Walsh history
// Robert Grumbine 15 July 1999.
//   to 17 April 2001
// Note (ncepgrids.h) that Walsh naming of i/j are different
//  that RG method.

#define NMONTHS (12 * 18)
#define NONVAL  224.

void walshtof(metricgrid<int> &c, metricgrid<float> &f, float &nonval) ;

extern void walshtof(metricgrid<char> &c, metricgrid<float> &f) ;
template <class T>
extern void getland(metricgrid<T> &land, metricgrid<T> &fconc, 
                    float nonval, float landval) ;


template <class T>
extern float dot(mvector<T> &x, mvector<T> &y) ;
template <class T>
extern float dot(mvector<T> &x, mvector<T> &y, T landval) ;
#include "subs.C"

int main(void) {
  FILE *fin, *fout;
  southgrid<float> nh, landncep;
  southgrid<unsigned char> ncepref;
  southwalsh<int> conc;
  southwalsh<float> land, fconc[NMONTHS];
  southwalsh<float> month[12], var, avg, tmp;
  mvector<float> time1(NMONTHS), time2(NMONTHS); 
  float landmask = 157, lnonval = NONVAL;
  ijpt loc;
  latpt ll_loc;
  int i;
  palette<unsigned char> gg(19, 65);
  char fname[900];
  #ifdef W3LIB
    char *grib;
    int lgrib = 0;
    tm  date;
    time_t secs;
    FILE *fgrib;
    int parmno = 91, mxbit = 8, depth = 0, lead = 0;
  #endif


// Open data file and initialize months for averaging:
  fin = fopen("southern-73-90.dat","r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open the arctic concentration file\n");
    return 1;
  }
  for(i = 0; i < 12; i++) {
    month[i].set(0.0);
  }

//Read in data, convert to float, and construct monthly averages:
  i = 0;
  loc.i = 39;
  loc.j = 39;
  while (i < NMONTHS ) {
    conc.set(0);
    conc.walshread_i(fin);
    walshtof(conc, fconc[i], landmask);
    printf("Average %8.4f %f\n", fconc[i].average(landmask), 
            fconc[i].operator[](loc) ); 
    month[ (i%12) ] += fconc[i];
    if (i == 0) {
      getland(land, fconc[i], lnonval, landmask);
      //land.printer(stdout);
      printf("landmask = %f\n",land.gridmax() ); fflush(stdout);
      avg = land;
      if (avg.average() < 2.56) avg *= 100.;
      avg.colorproc(land, 7, 65, std_ice_coloring);
      avg.xpm("land.xpm", 1, gg);
      avg.set(0.0);
    }
///////// Testing: look for anomalous concentration values:
  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
    ll_loc = land.locate(loc);
    if (ll_loc.lon < 0.) ll_loc.lon += 360.;
    if (ll_loc.lon > 280. && ll_loc.lon < 310.
        && fconc[i][loc] != 0. && fconc[i][loc] != landmask) {
      printf("%4d  %3d %3d  %6.2f %5.2f  %5.1f %5.1f\n",i, loc.i, loc.j, 
                  ll_loc.lon, ll_loc.lat, land[loc], fconc[i][loc]);
    }
  }
  } 

    i += 1;
  }
  fclose(fin);
  fflush(stdout);

// At this point, have the entire time series in one large(ish) array
// Find the monthly averages. 
  avg.set(0.0);
  for (i = 0; i < 12; i++) {
    month[i] /= (NMONTHS / 12);
    avg += month[i];
    //Note that this is required because the averaging process of 
    //  floating point numbers is not exact.
    //landmask = month[i].gridmax();
    printf("Average for month %d is %f\n", i, month[i].average(landmask) );
  }
  fflush(stdout);
  avg /= 12.;
  fout = fopen("avg.bin","w");
  avg.binout(fout);
  fclose(fout);
// Grib:
    #ifdef W3LIB
      date.tm_year = 0;
      date.tm_mon  = 0;
      date.tm_mday = 0;
      date.tm_hour = 0;
      date.tm_min  = 0;
      date.tm_sec  = 0;
      avg.pds.set_precision(0.01);
      avg.pds.set_time(date.tm_year, date.tm_mon, date.tm_mday,
                       date.tm_hour, date.tm_min);
      sprintf(fname, "swalshavg.%4d%02d.grib",1973 + i/12, i%12 + 1);
      fout = fopen(fname, "w");
      grib = new char [ (avg.xpoints()*avg.ypoints())/8 + 200 ];
      avg /= 100.;
      avg.gribit(parmno, depth, lead, grib, lgrib, mxbit);
      fwrite(grib, sizeof(char), lgrib, fout);
      fclose(fout);
      avg *= 100.;
    #endif
  if (avg.average() < 2.56) avg *= 100.;
  avg.colorproc(land, 7, 65, std_ice_coloring);
  avg.xpm("avg.xpm", 1, gg);

// Remove the average - by month:
  printf("About to try to remove the monthly averages\n"); fflush(stdout);
  for (i = 0; i < NMONTHS; i++) {
    fconc[i] -= month[ i%12 ] ;
    printf("Demeaned average for month %d is %f\n", i, 
                                          fconc[i].average(landmask) );
  }
  fflush(stdout);


// Compute the pointwise variances -- whole series
  printf("About to try to compute the local variances\n"); fflush(stdout);
  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
     for (i = 0; i < time1.xpoints() ; i++) {
      time1[i] = fconc[i].operator[](loc);
     }
     var[loc] = dot(time1, time1)/ time1.xpoints();
     printf("Variance at location %2d %2d is %f\n",loc.i, loc.j, var[loc]);
  }
  }
  printf("Maximum variance is %f\n",var.gridmax());
  tmp = var;
  tmp.scale();
  tmp.xpm("var.xpm", 7, gg);

// Build land/ocean mask from monthly average (157. -> land) and variance
//  (0 -> ocean/land);
      //avg = land;
      //if (avg.average() < 2.56) avg *= 100.;
      //avg.colorproc(land, 7, 65, std_ice_coloring);
      //avg.xpm("landpre.xpm", 1, gg);
      //avg.set(0.0);

  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
     if (month[0].operator[](loc) > 101. ) {
       land[loc] = landmask;
     }
     else {
       land[loc] = 0.0;
     }
  }
  }
  fout = fopen("walshland.bin","w");
  land.binout(fout);
  fclose(fout);
  // Grib:
    #ifdef W3LIB
      date.tm_year = 0;
      date.tm_mon  = 0;
      date.tm_mday = 0;
      date.tm_hour = 0;
      date.tm_min  = 0;
      date.tm_sec  = 0;
      land.pds.set_precision(0.01);
      land.pds.set_time(date.tm_year, date.tm_mon, date.tm_mday,
                       date.tm_hour, date.tm_min);
      sprintf(fname, "swalshland.%4d%02d.grib",1973 + i/12, i%12 + 1);
      fout = fopen(fname, "w");
      grib = new char [ (land.xpoints()*land.ypoints())/8 + 200 ];
      land /= 100.;
      land.gribit(parmno, depth, lead, grib, lgrib, mxbit);
      fwrite(grib, sizeof(char), lgrib, fout);
      fclose(fout);
      land *= 100.;
    #endif
      //avg = land;
      //if (avg.average() < 2.56) avg *= 100.;
      //avg.colorproc(land, 7, 65, std_ice_coloring);
      //avg.xpm("landpost.xpm", 1, gg);
      //avg.set(0.0);

  landncep.fromall(land, land, landmask, lnonval);
  // Intercompare the reference NCEP land mask and that interpolated from
  // Walsh:
  printf("about to open the ncep land mask file\n"); fflush(stdout);
  fin = fopen("sland.new", "r");
  ncepref.binin(fin);
  fclose(fin); 
  fflush(stdout);
  for (loc.j = 0; loc.j < landncep.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < landncep.xpoints(); loc.i++) {
    ll_loc = landncep.locate(loc);
    if (ll_loc.lon < 0.) ll_loc.lon += 360.; 
    if (ncepref[loc] != (unsigned char) 0 && landncep[loc] != 0.) {
      landncep[loc] = (float) ncepref[loc];
    }
    else if (landncep[loc] != lnonval && landncep[loc] != ncepref[loc]) {
      //printf("%3d %3d  %3d %3d\n",
      //     loc.i, loc.j, (int) ncepref[loc], (int) landncep[loc]);
      landncep[loc] = (float) ncepref[loc];
    }
    // Mask out low-latitudes as nonvals
    //if (ll_loc.lat > -56.0 && ll_loc.lon > 280. && ll_loc.lon < 310.) {
    //  landncep[loc] = NONVAL;
    //}
  }
  }
  fflush(stdout);
  // Print out walsh-NCEP grid of land and no data.  
  fout = fopen("walshonncep.bin","w");
  landncep.binout(fout);
  fclose(fout);
// Grib:
    #ifdef W3LIB
      date.tm_year = 0;
      date.tm_mon  = 0;
      date.tm_mday = 0;
      date.tm_hour = 0;
      date.tm_min  = 0;
      date.tm_sec  = 0;
      landncep.pds.set_precision(0.01);
      landncep.pds.set_time(date.tm_year, date.tm_mon, date.tm_mday,
                       date.tm_hour, date.tm_min);
      sprintf(fname, "slandncep.%4d%02d.grib",1973 + i/12, i%12 + 1);
      fout = fopen(fname, "w");
      grib = new char [ (landncep.xpoints()*landncep.ypoints())/8 + 200 ];
      landncep /= 100.;
      landncep.gribit(parmno, depth, lead, grib, lgrib, mxbit);
      fwrite(grib, sizeof(char), lgrib, fout);
      fclose(fout);
      landncep *= 100.;
    #endif

  // Write out an xpm of the land mask.
  nh = landncep;
  nh.colorproc(nh, 7, 65, std_ice_coloring);
  nh.xpm("slandncep.xpm", 1, gg);
  
  
///////////////////////////////////////////////////////////////
// Print out every single month, interpolated to the NCEP grid:
// 3 outputs: binary, grib (if available), and xpm
  for (i = 0; i < NMONTHS; i++) {
    fconc[i] += month[i%12];
    nh.fromall(fconc[i], land, landmask, NONVAL);

    // Apply the land and no-data masks constructed above:
    for (loc.j = 0; loc.j < landncep.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < landncep.xpoints(); loc.i++) {
      if (landncep[loc] > 100.) nh[loc] = landncep[loc];
    }
    }
    
// Binary:
    sprintf(fname, "smonth.%4d%02d.bin",1973 + i/12, i%12 + 1);
    fout = fopen(fname, "w");
    nh.binout(fout);
    fclose(fout);
// Grib:
    #ifdef W3LIB
      date.tm_year = 1973 + i/12;
      date.tm_mon  = i%12 + 1;
      date.tm_mday = 28;
      date.tm_hour = 0;
      date.tm_min  = 0;
      date.tm_sec  = 0;
      nh.pds.set_precision(0.01);
      nh.pds.set_time(date.tm_year, date.tm_mon, date.tm_mday,
                       date.tm_hour, date.tm_min);
      sprintf(fname, "smonth.%4d%02d.grib",1973 + i/12, i%12 + 1);
      fout = fopen(fname, "w");
      grib = new char [ (nh.xpoints()*nh.ypoints())/8 + 200 ];
      nh /= 100.;
      nh.gribit(parmno, depth, lead, grib, lgrib, mxbit);
      fwrite(grib, sizeof(char), lgrib, fout);
      fclose(fout);
      nh *= 100.;
    #endif
// XPM:
    nh.colorproc(landncep, 7, 65, std_ice_coloring);
    sprintf(fname, "smonth.%4d%02d.xpm",1973 + i/12, i%12 + 1);
    nh.xpm(fname, 1, gg);

// Restore fconc to deviation, for later computation of variance:
    fconc[i] -= month[i%12];
  }

///////////////////////////////////////////////////////////////
// Print out monthly mean fields -- with proper flags:
  for (i = 0; i < 12; i++) {
   //if (month[i].average() < 2.56) month[i] *= 100;
   nh.fromall(month[i], land, land.gridmax(), NONVAL);
   // mask out points which are more properly considered land (landncep):
   for (loc.j = 0; loc.j < nh.ypoints() ; loc.j++) {
   for (loc.i = 0; loc.i < nh.xpoints() ; loc.i++) {
     if (landncep[loc] > 100.) nh[loc] = landncep[loc];
   }
   }

//Binary:
    sprintf(fname, "avgmonth.%02d.bin",i%12 + 1);
    fout = fopen(fname, "w");
    nh.binout(fout);
    fclose(fout);
//Grib:
    #ifdef W3LIB
      date.tm_year = 0;  // As these are climatologies, go for the year 0
      date.tm_mon  = i%12 + 1;
      date.tm_mday = 28;
      date.tm_hour = 0;
      date.tm_min  = 0;
      date.tm_sec  = 0;
      nh.pds.set_precision(0.01);
      nh.pds.set_time(date.tm_year, date.tm_mon, date.tm_mday,
                       date.tm_hour, date.tm_min);
      sprintf(fname, "avgmonth.%02d.grib",i%12 + 1);
      fout = fopen(fname, "w");
      grib = new char [ (nh.xpoints()*nh.ypoints())/8 + 200 ];
      nh /= 100.;
      nh.gribit(parmno, depth, lead, grib, lgrib, mxbit);
      fwrite(grib, sizeof(char), lgrib, fout);
      fclose(fout);
      nh *= 100.;
    #endif
//XPM:
   sprintf(fname, "sh.%02d.xpm", i);
   nh.colorproc(landncep, 7, 65, std_ice_coloring);
   nh.xpm(fname, 1, gg);
   month[i].set(0.);  // This is for next step:
  }

///////////////////////////////////////////////////////////
// Now look for the monthly variances:
  for (i = 0; i < NMONTHS; i++) { 
    tmp = fconc[i];
    tmp *= tmp;
    month[i%12] += tmp;
  }
  for (i = 0; i < 12; i++) {
    month[i%12] /= (float) NMONTHS/(float) 12 ;
  }
  for (i = 0; i < 12; i++) {
    nh.fromall(month[i], land, land.gridmax(), NONVAL);
    printf("month %2d max, min var = %f %f\n",i+1, nh.gridmax(), nh.gridmin() );
   // mask out points which are more properly considered land (landncep):
   for (loc.j = 0; loc.j < nh.ypoints() ; loc.j++) {
   for (loc.i = 0; loc.i < nh.xpoints() ; loc.i++) {
     if (landncep[loc] > 100.) nh[loc] = landncep[loc];
   }
   }
//Binary:
    sprintf(fname, "varmonth.%02d.bin",i%12 + 1);
    fout = fopen(fname, "w");
    nh.binout(fout);
    fclose(fout);
//Grib:
    #ifdef W3LIB
      date.tm_year = 0;  // As these are climatologies, go for the year 0
      date.tm_mon  = i%12 + 1;
      date.tm_mday = 28;
      date.tm_hour = 0;
      date.tm_min  = 0;
      date.tm_sec  = 0;
      nh.pds.set_precision(0.01);
      nh.pds.set_time(date.tm_year, date.tm_mon, date.tm_mday,
                       date.tm_hour, date.tm_min);
      sprintf(fname, "varmonth.%02d.grib",i%12 + 1);
      fout = fopen(fname, "w");
      grib = new char [ (nh.xpoints()*nh.ypoints())/8 + 200 ];
      nh.gribit(parmno, depth, lead, grib, lgrib, mxbit);
      fwrite(grib, sizeof(char), lgrib, fout);
      fclose(fout);
    #endif
//XPM:
   month[i].scale();
   sprintf(fname, "shvar.%02d.xpm", i);
   nh.colorproc(landncep, 7, 65, std_ice_coloring);
   nh.xpm(fname, 1, gg);
  }

  return 0;
} 

////////////////////////////////////////////////////////////////////////
void walshtof(metricgrid<int> &c, metricgrid<float> &f, float &nonval) {
// Translate out of walsh int encoding to floating point concentrations
  ijpt loc;

  for (loc.j = 0; loc.j < c.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < c.xpoints() ; loc.i++) {
     f[loc] = c[loc]*10.;
     if (f[loc] > 110.) f[loc] = nonval;
  }
  }

  return;
}
