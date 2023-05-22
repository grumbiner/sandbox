#include <ctype.h>
#include <stdlib.h>
#include "vector.h"
#include "ncepgrids.h"

// Program to work with Walsh data sets/climatology
// Robert Grumbine 15 July 1999.
// Note (ncepgrids.h) that Walsh naming of i/j are different
//  that RG method.
// Variant: Compute conditional concentrations and statistics

// Must define the pole of interest in the compilation, with 
//   -DNORTH or -DSOUTH

#ifdef NORTH
  #define NYEARS    90
  #define DATA_FILE "arctic-concentration-01-90.dat"
  #define DATA_SOURCE "arctic-concentration-01-90.source"
  #define FIRST_YEAR 1901
  #define LOCI 35
  #define LOCJ 24
  #define HEM "nh"
  #define GRID northgrid
  #define WALSH northwalsh
  #define LANDMASK 1.50
#else
  #define NYEARS 18
  #define DATA_FILE "southern-73-90.dat"
  #define FIRST_YEAR 1973
  #define LOCI 39
  #define LOCJ 39
  #define HEM "sh"
  #define GRID  southgrid
  #define WALSH southwalsh
  #define LANDMASK 9.9
#endif

#define NMONTHS (NYEARS*12)
#define LAGMAX    73

#include "subs.C"


int main(void) {
  GRID<float> nh;
  GRID<int> inh;

  #ifdef NORTH
    WALSH<char> conc;
  #else
    WALSH<int> conc;
  #endif
  WALSH<int> iland, itmp, numobs, obs_mon[12];
  WALSH<float> land, tland, fconc[NMONTHS];
  WALSH<float> month[12], covar[LAGMAX], var, avg, correl, tmp;
  WALSH<float> soicor[2*LAGMAX - 1 ];
  time_series<float> times(NMONTHS), soi(NMONTHS);
  vector<float> time1(NMONTHS), time2(NMONTHS), time_mon[12];

  FILE *fin, *sourcein, *fout, *soiout;
  float landmask = LANDMASK, rij, nonval = LANDMASK, minr2 = 0.08;
  ijpt loc, loc2;
  latpt lloc1, lloc2;
  int i, j;
  char fname[900];
  palette<unsigned char> gg(19, 65), condit(14);
  int (*condition_fn)(int, int, int, int);

// Start reading in concentration field:
  fin = fopen(DATA_FILE, "r"); 
  if (fin == (FILE *) NULL) {
    printf("Failed to open the arctic concentration file\n");
    return 1;
  }
  #ifdef NORTH
  sourcein = fopen(DATA_SOURCE,"r");
  if (sourcein == (FILE *) NULL) {
    printf("Failed to open the arctic data source file\n");
    return 1;
  }
  #endif
  soiread(soi);
//  //soi.printer(stdout);
//  soiout = fopen("soiout", "w");
//  { float ref, tsoi;
//    fprintf(soiout, "Soi autocorrelation vs. lags\n");
//    j = 0;
//    ref = soi.autocovary(j);
//    for (j = 1; j < LAGMAX; j++) {
//      tsoi = soi.autocovary(j)/ref;
//      printf("Soi autocor lag %2d %f\n",j, tsoi);
//    }
//  }
//  fflush(soiout);

// Set up color tables and functions
  condition_table(condit);
  condition_fn = &condition_color; 

/////////////////////////////////////////
// Read in, process, and convert to standard grid every month of info.
/////////////////////////////////////////

  for(i = 0; i < 12; i++) {
    month[i].set(0.0);
    time_mon[i].resize(NMONTHS/12);
    obs_mon[i].set(0);
  }
  numobs.set(0);

  i = 0;
  loc.i = LOCI;
  loc.j = LOCJ;
  while (i < NMONTHS ) {
    conc.set('0');
    #ifdef NORTH
      conc.walshread_c(fin);
      walshctof(conc, fconc[i]);
    #else
      conc.walshread_i(fin);
      walshitof(conc, fconc[i]);
    #endif
    if (i == 0) {
       findland(land, fconc[i], landmask);
    }
    else {
       findland(tland, fconc[i], landmask);
       if (! (land == tland) ) {
         printf("land changed in month %d\n",i);
       }
    }
    flagger(fconc[i], nonval, 0.0);
    flagger(fconc[i], nonval, landmask);
    counter(numobs, fconc[i], nonval); 
    counter(obs_mon[i%12], fconc[i], nonval); 
    //// Now print out the figure:
    //tmp = fconc[i];
    //tmp += land;
    //tmp *= 100;
    //nh.fromall(tmp, land, land.gridmax(), 224.);
    //// Note that to put in land, need to re-force the issue.  
    //// Leave undefined here.
    //nh.colorproc(land, 7, 65, ice_coloring);
    //sprintf(fname, "%sice%4d%02d.xpm",HEM, FIRST_YEAR+i/12, i%12 + 1);
    //nh.xpm(fname, 1, gg);
    #ifdef W3LIB
       nh.gribit
    #endif

    i += 1;
  }
  fclose(fin);
  fflush(stdout);

// At this point, have the entire time series in one large(ish) array,
//   and a land mask is saved for reference.

// Find the monthly averages, ignoring months (points) of nodata:
  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
     for (i = 0; i < time1.xpoints() ; i++) {
       time_mon[i%12].operator[](i/12) = fconc[i].operator[](loc);
     }
     for (j = 0; j < 12; j++) {
       month[j].operator[](loc) = time_mon[j].average(nonval);
     }
  }
  }

// Annual average
  avg.set(0.0);
  numobs.set(0);
  for (i = 0; i < 12; i++) {
    numobs += obs_mon[i];
    avg += month[i];
    //Note that the following is required because the averaging process of 
    //  floating point numbers is not exact.
    printf("Average for %2d is %f\n", i, month[i].average(landmask) );
  }
  fflush(stdout);
  fflush(soiout);
  avg /= 12.;
  avg *= 100.;
  land *= 100.;
  for (i = 0; i < land.xpoints()*land.ypoints(); i++) {
    iland[i] = (int) (0.5 + land[i]) ;
  }
  avg.colorproc(land, 7, 65, ice_coloring);
  avg.xpm("avg.xpm", 1, gg);
  tmp = avg;
  nh.fromall(tmp, land, land.gridmax(), 224.);
  nh.colorproc(land, 7, 65, ice_coloring);
  nh.xpm("annual.xpm", 1, gg);
  printf("numobs max, min, average %d %d %d  land %d %d\n",
    numobs.gridmax(), numobs.gridmin(), numobs.average(), iland.gridmax(), 
    iland.gridmin() );
  numobs *= 100; // increase for rounding in next line
  numobs /= NMONTHS;
  numobs.colorproc(iland, iland.gridmax(), 0, condition_fn);
  numobs.xpm("numobs.xpm", 1, condit);
//  itmp = numobs;
//  inh.fromall(itmp, iland, iland.gridmax(), 224);
//  inh.colorproc(iland, iland.gridmax(), 0, condition_fn);
//  inh.xpm("totalcount.xpm", 1, condit);
  
  for (i = 0; i < 12; i++) {
    numobs = obs_mon[i];
    numobs *= 100; // for percent
    numobs /=  NYEARS; // for the 90 years record
    sprintf(fname,"$smonth.count.%02d.xpm", HEM, i+1);
    numobs.colorproc(iland, iland.gridmax(), 0, condition_fn);
    numobs.xpm(fname, 1, condit);

    sprintf(fname,"month.cavg.%02d.xpm", i+1); 
    tmp = month[i];
    tmp.colorproc(land, 7, 65, ice_coloring);
    tmp.xpm(fname, 1, gg);
  }
  land /= 100.; //restore

// Print out monthly conditional mean fields
  for (i = 0; i < 12; i++) {
   sprintf(fname, "%s.%02d.xpm", HEM, i+1);
   tmp = month[i]; 
   tmp += land;
   tmp *= 100;
   nh.fromall(tmp, land, landmask, 0.);
   nh.xpm(fname, 8, gg);
  }
  fflush(stdout);

  return 0;

// Remove the average - by month:
  for (i = 0; i < NMONTHS; i++) {
    condsub(fconc[i], month[i%12], landmask );
    printf("Demeaned month %9.4f is %6.3f \n", FIRST_YEAR+((float)(i+1))/12., 
                                          fconc[i].average(landmask));
  }
  fflush(stdout); 


// Compute the pointwise variances
  covar[0].set(0.);
  for (i = 1; i < LAGMAX; i++) {
    covar[i].set(0.);
  }
  printf("About to try to compute the local variances\n"); fflush(stdout);
  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
     lloc1 = land.locate(loc);
     for (i = 0; i < time1.xpoints() ; i++) {
      time1[i] = fconc[i].operator[](loc);
     }
     if (time1.complete(landmask) != 0.) {
       var[loc] = dot(time1, time1, landmask)/ (time1.xpoints()*time1.complete(landmask));
       printf("Variance at %2d %2d, %6.3f %6.3f is %f %6.3f complete\n",
                loc.i, loc.j, lloc1.lon, lloc1.lat, var[loc], 
                time1.complete(landmask)    );
     }
     else {
       var[loc] = 0.0;
     }
     if ( land[loc] < 1.00 && var[loc] > 0. && 
                              time1.complete(landmask) > 1./12./2. ) {
       float ref, ref_soi, soitmp;
       times.set(time1);
       j = 0;
       ref = times.autocovary(landmask, j);
       ref_soi = soi.autocovary(j);
       if (ref > 0.) {
         // Look at ice leading soi
         soitmp = times.crossvary(soi, landmask, j) / sqrt(ref*ref_soi);
         soicor[j+LAGMAX - 1].operator[](loc) = soitmp;
         fprintf(soiout, "Soi crossvar %2d %2d lag %2d %6.3f\n", 
             loc.i, loc.j, j, soitmp );
         covar[0].operator[](loc) = 1.0;
         for (j = 1; j < LAGMAX; j++) {
            covar[j].operator[](loc) = times.autocovary(landmask, j)/ref ;
            printf("Autocov %2d %2d  %2d lag %6.3f\n",loc.i, loc.j, j, 
                   covar[j].operator[](loc)  );  
            soitmp = times.crossvary(soi, landmask, j) / sqrt(ref*ref_soi);
            soicor[j+LAGMAX - 1].operator[](loc) = soitmp;
            fprintf(soiout, "Soi crossvar %2d %2d %2d lag %6.3f\n", 
                     loc.i, loc.j, j, soitmp );
         }
         // Also look at soi leading the ice
         for (j = 1; j < LAGMAX; j++) {
            soitmp = soi.crossvary(times, landmask, j) / sqrt(ref*ref_soi);
            soicor[ -j+LAGMAX - 1].operator[](loc) = soitmp;
            fprintf(soiout, "Soi crossvar %2d %2d %3d lag %6.3f\n", 
              loc.i, loc.j, -j, soitmp );
         }

       } // End of if ref != 0.
     }
  }
  }
  printf("Maximum variance is %f\n",var.gridmax());
  tmp = var;
  tmp *= 1./var.gridmax();
  tmp += land;
  tmp.scale();
  tmp.xpm("var.xpm", 7, gg);
  for (j = 1; j < LAGMAX; j++) {
    printf("%d %f max covar\n",j, covar[j].gridmax() );
    covar[j] += 1.0; // scale in to positives
    covar[j] *= 100.; // 0-300;
    sprintf(fname, "covar%02d.xpm",j);
    covar[j].xpm(fname, 9, gg);
  }
  fflush(stdout); 
  fflush(soiout);
  for (j = -LAGMAX + 1; j < LAGMAX; j++) {
    soicor[j + LAGMAX - 1] += 0.4; // +- .3 is normal range
    soicor[j + LAGMAX - 1] *= 200.; 
    sprintf(fname, "soicor%03d.xpm",j);
    soicor[j + LAGMAX - 1].xpm(fname, 8, gg);
  }

// Build land/ocean mask from monthly average (1.57 -> land) and variance
//  (0 -> ocean/land);
  land *= 100.;
  land.xpm("land.xpm", 12, gg);

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
   sprintf(fname, "%svar.%02d.xpm", HEM, i);
   printf("Max variation in month %d is %f\n",i+1, month[i].gridmax() );
   month[i].scale();
   nh.fromall(month[i], land, land.gridmax(), 0.);
   nh.xpm(fname, 12, gg);
  }




// Now have figures in hand to start computing the cross-covariances/
//  correlations

// Now vary through the grid, taking only non-land points which don't have
//   zero variance (would lead to undefineds).  (This cuts out 3/4ths of the
//   points).
  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
    if (land[loc] > 1.0 || var[loc] == 0.) continue;

    for (i = 0; i < time1.xpoints() ; i++) {
     time1[i] = fconc[i].operator[](loc);
    }
    correl.set(0.0);
    for (loc2.j = 0; loc2.j < land.ypoints(); loc2.j++) {
    for (loc2.i = 0; loc2.i < land.xpoints(); loc2.i++) {
      if (land[loc2] > 1.0 || var[loc2] == 0.) continue;
      if (loc2 == loc) continue;
      for (i = 0; i < time2.xpoints() ; i++) {
       time2[i] = fconc[i].operator[](loc2);
      }
      rij = dot(time1, time2, landmask)/( (float)NMONTHS);
      rij /= sqrt(var[loc]*var[loc2]);
      correl[loc2] = rij;
      if ( (rij*rij) > 0.25 ) { 
        lloc1 = correl.locate(loc);
        lloc2 = correl.locate(loc2);
        printf("cor %6.3f  %2d %2d to %2d %2d  %6.3f %6.3f to %6.3f %6.3f\n", 
                        rij, loc.i, loc.j, 
                        loc2.i, loc2.j, lloc1.lat, lloc1.lon, 
                        lloc2.lat, lloc2.lon); 
      }
    }
    } // end of loc2

    tmp = correl;
    tmp *= correl;
    printf("%2d %2d grid-averaged correl, r2 %6.3f %6.3f\n",loc.i, loc.j,
      correl.average(), tmp.average() );

    sprintf(fname, "correl.%02d%02d", loc.i, loc.j);
    fout = fopen(fname, "w");
    if (fout == (FILE *) NULL) {
      cout << "Failed to open the output file " << fname << endl;
      return 3;
    }
    correl.binout(fout);
    fclose(fout);
    correl *= correl;  //Show R^2 for graphics
    if (correl.gridmax() > minr2) {
      correl *= 100.;
      correl += land;  //For figures;
      sprintf(fname, "cor.%02d%02d.xpm", loc.i, loc.j);
      correl.xpm(fname, 9, gg);
    }

  }
  } // end of loc

  return 0;

} 

