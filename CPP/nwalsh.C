#include <ctype.h>
#include <stdlib.h>
#include "ncepgrids.h"
#include "time_series.h"

// Program to work with Walsh data sets/climatology
// Robert Grumbine 15 July 1999.
// Note (ncepgrids.h) that Walsh naming of i/j are different
//  that RG method.
// 10 April 2000

#define NMONTHS (12 * 90)
#define LAGMAX    73

#include "subs.C"
extern void walshtof(metricgrid<char> &c, metricgrid<float> &f) ;
template <class T>
extern float dot(mvector<T> &x, mvector<T> &y) ;
template <class T>
extern float dot(mvector<T> &x, mvector<T> &y, T landval) ;

int main(void) {
  FILE *fin, *fout;
  northgrid<float> nh;
  northwalsh<char> conc;
  northwalsh<float> land, fconc[NMONTHS];
  northwalsh<float> month[12], covar[LAGMAX], var, avg, correl, tmp;
  northwalsh<float> soicor[2*LAGMAX - 1 ];
  mvector<float> time1(NMONTHS), time2(NMONTHS); 
  time_series<float> times(NMONTHS), soi(NMONTHS);
  float landmask = 1.57, rij;
  ijpt loc, loc2;
  latpt lloc1, lloc2;
  int i, j;
  palette<unsigned char> gg(19, 65);
  char fname[900];

// Start reading in concentration field:
  fin = fopen("arctic-concentration-01-90.dat","r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open the arctic concentration file\n");
    return 1;
  }
  soiread(soi, 1901);

  for(i = 0; i < 12; i++) {
    month[i].set(0.0);
  }

  i = 0;
  loc.i = 35;
  loc.j = 24;
  while (i < NMONTHS ) {
    conc.set('0');
    conc.walshread_c(fin);
    walshtof(conc, fconc[i]);
//    printf("Average %8.4f %f\n", fconc[i].average(landmask), 
//            fconc[i].operator[](loc) ); 
    month[ (i%12) ] += fconc[i];
    i += 1;
  }
  fclose(fin);
  fflush(stdout);
  
// At this point, have the entire time series in one large(ish) array

// Find the monthly averages, ignoring months of nodata:
  avg.set(0.0);
  for (i = 0; i < 12; i++) {
    month[i] /= (NMONTHS / 12);
    avg += month[i];
    //Note that this is required because the averaging process of 
    //  floating point numbers is not exact.
    landmask = month[i].gridmax();
    printf("Average for month %d is %f\n", i, month[i].average(landmask) );
  }
  fflush(stdout);
  avg /= 12.;
  avg *= 100.;
  avg.xpm("avg.xpm", 9, gg);

// Remove the average - by month:
  printf("About to try to remove the monthly averages\n"); fflush(stdout);
  for (i = 0; i < NMONTHS; i++) {
    fconc[i] -= month[ i%12 ] ;
    printf("Demeaned average for month %d is %f\n", i, 
                                          fconc[i].average(landmask) );
  }
  fflush(stdout);



// Compute the pointwise variances
  covar[0].set(1.1);
  for (i = 1; i < LAGMAX; i++) {
    covar[i].set(1.1);
  }
  printf("About to try to compute the local variances\n"); fflush(stdout);
  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
     for (i = 0; i < time1.xpoints() ; i++) {
      time1[i] = fconc[i].operator[](loc);
     }
     var[loc] = dot(time1, time1)/ time1.xpoints();
     printf("Variance at location %2d %2d is %f\n",loc.i, loc.j, var[loc]);
     if ( land[loc] < 1.00 && var[loc] > 0. ) {
       float ref, ref_soi, soitmp;
       times.set(time1);
       j = 0;
       ref = times.autocovary(j);
       ref_soi = soi.autocovary(j);
       if (ref != 0.) {
         // Look at ice leading soi
         soitmp = times.crossvary(soi, j) / sqrt(ref*ref_soi);
         soicor[j+LAGMAX - 1].operator[](loc) = soitmp;
         printf("Soi crossvar %2d %2d %2d lag %6.3f\n", loc.i, loc.j, j,
                soitmp );
         covar[0].operator[](loc) = 1.0;
         for (j = 1; j < LAGMAX; j++) {
            covar[j].operator[](loc) = times.autocovary(j)/ref ;
            printf("Autocov %2d %2d  %2d lag %6.3f\n",loc.i, loc.j, j,
                   covar[j].operator[](loc)  );
            soitmp = times.crossvary(soi, j) / sqrt(ref*ref_soi);
            soicor[j+LAGMAX - 1].operator[](loc) = soitmp;
            printf("Soi crossvar %2d %2d %2d lag %6.3f\n", loc.i, loc.j, j,
                   soitmp );
         }
         // Also look at soi leading the ice
         for (j = 1; j < LAGMAX; j++) {
            soitmp = times.crossvary(soi, j) / sqrt(ref*ref_soi);
            soicor[ -j+LAGMAX - 1].operator[](loc) = soitmp;
            printf("Soi crossvar %2d %2d %3d lag %6.3f\n", loc.i, loc.j, -j,
                     soitmp );
         }

       } // End of if ref != 0.
     }
  }
  }
  printf("Maximum variance is %f\n",var.gridmax());
  land = var;
  land.scale();
  land.xpm("var.xpm", 7, gg);
  for (j = 1; j < LAGMAX; j++) {
    covar[j] += 1.0; // scale in to positives
    covar[j] *= 100.; // 0-300;
    sprintf(fname, "covar%02d.xpm",j);
    covar[j].xpm(fname, 12, gg);
  }
  for (j = -LAGMAX + 1; j < LAGMAX; j++) {
    soicor[j + LAGMAX - 1] += 0.5; // +- .3 is normal range
    soicor[j + LAGMAX - 1] *= 100.; 
    sprintf(fname, "soicor%03d.xpm",j);
    soicor[j + LAGMAX - 1].xpm(fname, 6, gg);
  }

// Build land/ocean mask from monthly average (1.57 -> land) and variance
//  (0 -> ocean/land);
  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
     if (month[0].operator[](loc) > 1.01 ) {
       land[loc] = landmask;
     }
     else {
       land[loc] = 0.0;
     }
  }
  }
  land *= 100.;
  land.xpm("land.xpm", 12, gg);
// Print out monthly mean fields
  for (i = 0; i < 12; i++) {
   sprintf(fname, "nh.%02d.xpm", i);
   month[i] *= 100;
   nh.fromall(month[i], land, land.gridmax(), 0.);
   nh.xpm(fname, 7, gg);
   month[i].set(0.);
  }
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
      rij = dot(time1, time2)/( (float)NMONTHS);
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
    correl *= correl;  //Show R^2 for graphics
    correl *= 100.;
    correl += land;  //For figures;
    sprintf(fname, "cor.%02d%02d.xpm", loc.i, loc.j);
    correl.xpm(fname, 9, gg);
    fclose(fout);

  }
  } // end of loc

  return 0;
} 
