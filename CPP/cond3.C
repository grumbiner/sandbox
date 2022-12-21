#include <ctype.h>
#include <stdlib.h>
#include "ncepgrids.h"
#include "time_series.h"

// Program to work with Walsh data sets/climatology
// Robert Grumbine 15 July 1999.
// Note (ncepgrids.h) that Walsh naming of i/j are different
//  that RG method.
// Variant: Compute conditional concentrations and statistics

#define NYEARS    90
#define NMONTHS (NYEARS*12)
#define LAGMAX    73
#define MAXSERIES 700

#include "subs.C"
extern void findland(metricgrid<float> &land, metricgrid<float> &conc, float landval) ;
template<class T>
extern void shorten(mvector<time_series<T> > &regress, time_series<T> &target, 
  int lastn, int nfull, int shift);   
extern void condsub(metricgrid<float> &orig, metricgrid<float> &avg, float landval) ;
template <class T>
extern void descent(mvector<time_series<T> > &regress, time_series<T> &target, 
                    mvector<ijpt> &points, int npoints, T &flag, int lag);
extern void walshtof(metricgrid<char> &c, metricgrid<float> &f) ;
template <class T>
extern float dot(mvector<T> &x, mvector<T> &y) ;
template <class T>
extern float dot(mvector<T> &x, mvector<T> &y, T landval) ;


int main(int argc, char *argv[]) {
  northgrid<float> nh;

  northwalsh<char> conc;
  northwalsh<int> iland, numobs, obs_mon[12];
  northwalsh<float> land, fconc[NMONTHS];
  northwalsh<float> month[12], covar[LAGMAX], var, avg, correl, tmp;
  northwalsh<float> soicor[2*LAGMAX - 1 ];
  time_series<float> times(NMONTHS), soi(NMONTHS);
  time_series<float> time1(NMONTHS), time2(NMONTHS);
  mvector<float> time_mon[12];
  mvector<ijpt> fullpts(conc.xpoints()*conc.ypoints() );
  mvector<time_series<float> > regress(MAXSERIES); // mvector of time series -- for multiple regression
  int nfullpts = 0;


  FILE *fin, *sourcein, *fout, *soiout;
  float landmask = 1.50, rij, nonval = 1.50, minr2 = 0.08;
  ijpt loc, loc2;
  latpt lloc1, lloc2;
  int i, j;
  char fname[900];
  palette<unsigned char> gg(19, 65), condit(14);
  int (*condition_fn)(int, int, int, int);

// Start reading in concentration field:
  fin = fopen("arctic-concentration-01-90.dat","r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open the arctic concentration file\n");
    return 1;
  }
  sourcein = fopen("arctic-concentration-01-90.source","r");
  if (sourcein == (FILE *) NULL) {
    printf("Failed to open the arctic data source file\n");
    return 1;
  }
  soiread(soi, 1901);
  //soi.printer(stdout);
  soiout = fopen("soiout", "w");
  { float ref, tsoi;
    fprintf(soiout, "Soi autocorrelation vs. lags\n");
    j = 0;
    ref = soi.autocovary(j);
    for (j = 1; j < LAGMAX; j++) {
      tsoi = soi.autocovary(j)/ref;
      printf("Soi autocor lag %2d %f\n",j, tsoi);
    }
  }
// Set up color tables and functions
  condition_table(condit);
  condition_fn = &condition_color; 


  for(i = 0; i < 12; i++) {
    month[i].set(0.0);
    time_mon[i].resize(NMONTHS/12);
    obs_mon[i].set(0);
  }
  numobs.set(0);

  for (i = 0; i < MAXSERIES; i++) {
     regress[i].resize(NMONTHS);
  }

  i = 0;
  loc.i = 35;
  loc.j = 24;
  while (i < NMONTHS ) {
    conc.set('0');
    conc.walshread_c(fin);
    walshtof(conc, fconc[i]);
    if (i == 0) findland(land, fconc[i], landmask);
    flagger(fconc[i], nonval, 0.0);
    flagger(fconc[i], nonval, landmask);
    counter(numobs, fconc[i], nonval); 
    counter(obs_mon[i%12], fconc[i], nonval); 
    i += 1;
  }
  fclose(fin);
  fflush(stdout);
  fflush(soiout);
// At this point, have the entire time series in one large(ish) array,
//   and a land mask is saved for reference.


// Find the monthly averages, ignoring months of nodata:
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
  avg.colorproc(land, 7, 65, std_ice_coloring);
  avg.xpm("avg.xpm", 1, gg);
  printf("numobs max, min, average %d %d %d  land %d %d\n",
    numobs.gridmax(), numobs.gridmin(), numobs.average(), iland.gridmax(), 
    iland.gridmin() );
  numobs *= 100; // increase for rounding in next line
  numobs /= NMONTHS;
  numobs.colorproc(iland, iland.gridmax(), 0, condition_fn);
  numobs.xpm("numobs.xpm", 1, condit);
  for (i = 0; i < 12; i++) {
    numobs = obs_mon[i];
    numobs *= 100; // for percent
    numobs /=  NYEARS; // for the 90 years record
    sprintf(fname,"month.count.%02d.xpm", i+1);
    numobs.colorproc(iland, iland.gridmax(), 0, condition_fn);
    numobs.xpm(fname, 1, condit);

    sprintf(fname,"month.cavg.%02d.xpm", i+1); 
    tmp = month[i];
    tmp.colorproc(land, 7, 65, std_ice_coloring);
    tmp.xpm(fname, 8, gg);
  }
  land /= 100.; //restore

// Print out monthly conditional mean fields
  for (i = 0; i < 12; i++) {
   sprintf(fname, "nh.%02d.xpm", i+1);
   tmp = month[i]; 
   tmp += land;
   tmp *= 100;
   //nh.fromall(tmp, land, landmask, 0.);
   //nh.xpm(fname, 8, gg);
   sprintf(fname, "count.%02d.xpm", i);
   //obs_mon[i].xpm(fname, 5, gg);
  }
  fflush(stdout);


// Remove the average - by month:
  for (i = 0; i < NMONTHS; i++) {
    condsub(fconc[i], month[i%12], landmask );
    //printf("Demeaned month %9.4f is %6.3f \n", 1901.+((float)(i+1))/12., 
    //                                      fconc[i].average(landmask));
  }
  fflush(stdout); 

// Start working on the statistics relative to points.

// Try working on boolean correlation of points to soi:
  loc.i = atoi(argv[1]);
  loc.j = atoi(argv[2]);
  if (loc.i == 0 && loc.j == 0) {
    loc.i = 0;
    for (i = 0; i < time1.xpoints() ; i++) {
        time2[i] = soi[i];
        //printf("%d soi %f\n",i,time2[i]);
        if (soi[i] < -0.5) loc.i += 1;
    }
    printf("Number of times in 'soi' %d, series length %d\n",loc.i, time1.xpoints());
    loc.i = 0;
  }
  else {
    //Variant set time2 to be a particular point:
    for (i = 0; i < time2.xpoints(); i++) {
       time2[i] = fconc[i].operator[](loc);
    }
    printf("Series at pt %2d %2d\n",loc.i, loc.j);
    time2.printer(stdout);
  }

// for all series:
  printf("Autocorrelation of series at %2d, %2d\n",loc.i, loc.j);
  { int lag;
    i = 0;
    for (lag = 0; lag < LAGMAX; lag++) {
      float var2, rt;
      var2 = time2.autocovary(landmask, i);
      rt = time2.autocovary(landmask, lag) / var2;
      printf("pt %2d %2d lag %3d %6.3f\n",loc.i, loc.j, lag, rt); 
    }
  }

// Now try to cross correlate:
  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
     if (loc.i == atoi(argv[1]) && loc.j == atoi(argv[2]) ) continue;
     for (i = 0; i < time1.xpoints() ; i++) {
      time1[i] = fconc[i].operator[](loc);
     }
     if (time1.complete(landmask) != 0.) {
       printf("P(improve) at %3d %3d is %f\n",
             loc.i, loc.j, booltest(time1, time2) );
     }
     if (time1.complete(landmask) >= 1.-1./12.) {
// As we find the points that have sufficiently complete series, save
//   the location and count them. This will also make it possible to set
//   up the arrays for multiple regressions
       fullpts[nfullpts] = loc;
       regress[nfullpts] = time1;
       nfullpts += 1;

//alpha       printf("R at %3d %3d is %7.4f complete %6.3f %6.3f\n", loc.i, loc.j, 
//alpha           rt, time1.complete(landmask), time2.complete(landmask) ) ;
//alpha       //if (fabs(rt) > 0.04) {
//alpha         lag = 0;
//alpha         var1 = time1.autocovary(landmask, lag);
//alpha         var2 = time2.autocovary(landmask, lag);
//alpha         for (lag = -LAGMAX; lag < LAGMAX; lag++) {
//alpha            rt = time1.crossvary(time2, landmask, lag) / sqrt(var1*var2) ;
//alpha            printf("%3d %3d lag %4d cross vary %f\n",loc.i, loc.j, lag, rt);
//alpha            if (fabs(rt) > 0.1) {
//alpha              lloc1 = conc.locate(loc);
//alpha              printf("high %3d %3d  %7.2f %6.2f  %4d %7.4f\n",loc.i, loc.j, lloc1.lon, lloc1.lat, lag, rt);
//alpha            }
//alpha       //  }
//alpha       }
     }
  }
  }

  printf("Found %d points which were sufficiently complete\n",
       nfullpts );
  for (i = 0; i < nfullpts; i++) {
    printf("%3d %3d\n",fullpts[i].i, fullpts[i].j);
  }

// arg3 = length to take out, arg4 = how far from end of record to finish up
  shorten(regress, time2, atoi(argv[3]), nfullpts, atoi(argv[4]) );
  for (i = 0; i < 55; i++) {
    printf("About to try to construct multiple descent for lag %d\n",i);
    descent(regress, time2, fullpts, nfullpts, landmask, i);
  }


  return 0;

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
  //tmp.xpm("var.xpm", 7, gg);
  for (j = 1; j < LAGMAX; j++) {
    printf("%d %f max covar\n",j, covar[j].gridmax() );
    covar[j] += 1.0; // scale in to positives
    covar[j] *= 100.; // 0-300;
    sprintf(fname, "covar%02d.xpm",j);
    //covar[j].xpm(fname, 9, gg);
  }
  fflush(stdout); 
  fflush(soiout);
  for (j = -LAGMAX + 1; j < LAGMAX; j++) {
    soicor[j + LAGMAX - 1] += 0.4; // +- .3 is normal range
    soicor[j + LAGMAX - 1] *= 200.; 
    sprintf(fname, "soicor%03d.xpm",j);
    //soicor[j + LAGMAX - 1].xpm(fname, 8, gg);
  }

// Build land/ocean mask from monthly average (1.57 -> land) and variance
//  (0 -> ocean/land);
  land *= 100.;
  //land.xpm("land.xpm", 12, gg);

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
   sprintf(fname, "nhvar.%02d.xpm", i);
   printf("Max variation in month %d is %f\n",i+1, month[i].gridmax() );
   month[i].scale();
   //nh.fromall(month[i], land, land.gridmax(), 0.);
   //nh.xpm(fname, 12, gg);
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
