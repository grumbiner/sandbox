#include <ctype.h>
#include <stdlib.h>
#include "vector.h"
#include "ncepgrids.h"

// Program to work with Walsh data sets/climatology
// Robert Grumbine 15 July 1999.
// Note (ncepgrids.h) that Walsh naming of i/j are different
//  that RG method.
// Variant: Compute conditional concentrations and statistics

#define NYEARS    90
#define NMONTHS (NYEARS*12)
#define LAGMAX    73

void walshtof(metricgrid<char> &c, metricgrid<float> &f) ;
float dot(vector<float> &x, vector<float> &y) ;
float dot(vector<float> &x, vector<float> &y, float landval) ;
void soiread(time_series<float> &soi);
void flagger(metricgrid<float> &f, float nonval, float clobber); 
                //clobber is value to replace with nonval
void counter(metricgrid<int> &obscount, metricgrid<float> &conc, float nonval);
void findland(metricgrid<float> &land, metricgrid<float> &conc, float landval) ;
void condsub(metricgrid<float> &orig, metricgrid<float> &avg, float landval) ;

template <class T>
T condition_color(T x, T y, int c1, int c2) ;
template <class T>
float booltest(vector<T> &x, vector<T> &soi) ;
template <class T>
float r12(vector<T> &x, vector<T> &y, T mask) ;

void condition_table(palette<unsigned char> &x) ;

int main(void) {
  northgrid<float> nh;

  northwalsh<char> conc;
  northwalsh<int> iland, numobs, obs_mon[12];
  northwalsh<float> land, fconc[NMONTHS];
  northwalsh<float> month[12], covar[LAGMAX], var, avg, correl, tmp;
  northwalsh<float> soicor[2*LAGMAX - 1 ];
  time_series<float> times(NMONTHS), soi(NMONTHS);
  //vector<float> time1(NMONTHS), time2(NMONTHS), time_mon[12];
  time_series<float> time1(NMONTHS), time2(NMONTHS);
  vector<float> time_mon[12];

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
  soiread(soi);
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

// Try working on boolean correlation of points to soi:
//ORIG  loc.i = 0;
//ORIG  for (i = 0; i < time1.xpoints() ; i++) {
//ORIG      time2[i] = soi[i];
//ORIG      //printf("%d soi %f\n",i,time2[i]);
//ORIG      if (soi[i] < -0.5) loc.i += 1;
//ORIG  }
//ORIG  printf("Number of times in 'soi' %d, series length %d\n",loc.i, time1.xpoints());

  //Variant test, set time2 to be a particular piont, say 23, 30 (Beaufort)
  // 26,15 -> Laptev Sea
  loc.i = 26;
  loc.j = 15;
  for (i = 0; i < time2.xpoints(); i++) {
     time2[i] = fconc[i].operator[](loc);
  }
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

  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
     for (i = 0; i < time1.xpoints() ; i++) {
      time1[i] = fconc[i].operator[](loc);
     }
     if (time1.complete(landmask) != 0.) {
       printf("P(improve) at %3d %3d is %f\n",
             loc.i, loc.j, booltest(time1, time2) );
     }
     if (time1.complete(landmask) >= 0.9) {
       float var1, var2, rt = r12(time1, time2, landmask);
       int lag;
       printf("R at %3d %3d is %7.4f complete %6.3f %6.3f\n", loc.i, loc.j, 
           rt, time1.complete(landmask), time2.complete(landmask) ) ;
       //if (fabs(rt) > 0.04) {
         lag = 0;
         var1 = time1.autocovary(landmask, lag);
         var2 = time2.autocovary(landmask, lag);
         for (lag = -LAGMAX; lag < LAGMAX; lag++) {
            rt = time1.crossvary(time2, landmask, lag) / sqrt(var1*var2) ;
            printf("%3d %3d lag %4d cross vary %f\n",loc.i, loc.j, lag, rt);
            if (fabs(rt) > 0.1) {
              lloc1 = conc.locate(loc);
              printf("high %3d %3d  %7.2f %6.2f  %4d %7.4f\n",loc.i, loc.j, lloc1.lon, lloc1.lat, lag, rt);
            }
       //  }
       }
     }
  }
  }

  return 0;

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
   nh.fromall(tmp, land, landmask, 0.);
   nh.xpm(fname, 8, gg);
   sprintf(fname, "count.%02d.xpm", i);
   obs_mon[i].xpm(fname, 5, gg);
  }
  fflush(stdout);


// Remove the average - by month:
  for (i = 0; i < NMONTHS; i++) {
    condsub(fconc[i], month[i%12], landmask );
    printf("Demeaned month %9.4f is %6.3f \n", 1901.+((float)(i+1))/12., 
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


void walshtof(metricgrid<char> &c, metricgrid<float> &f) {
// Translate out of walsh character encoding to floating point concentrations
  ijpt loc;
  int tmpint;
  char *c1;

  c1 = new char[1];
  for (loc.j = 0; loc.j < c.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < c.xpoints() ; loc.i++) {
     
    //printf("character %2d %2d %1c\n",loc.i, loc.j, c[loc]);

    if (isdigit(c[loc]) ) {
      c1[0] = c[loc];
      tmpint = atoi(c1);
      f[loc] = ((float) tmpint) / 10.0;
    } 
    else if (c[loc] == '*') {
      f[loc] = 1.0;
    }
    else if (c[loc] == '.') {
      f[loc] = 1.50; // Land tag in RG systems
    }
    else {
      printf("Unknown character type %2d %2d %1c\n",loc.i, loc.j, c[loc]);
    }
  }
  }

  return;
}
float dot(vector<float> &x, vector<float> &y) {
  int i;
  float sum = 0.0;
  if (x.xpoints() != y.xpoints() ) {
    printf("cannot dot unequal sized vectors\n");
    return -1.;
  }
  for (i = 0; i < x.xpoints() ; i++) {
    sum += x[i]*y[i];
  }
  return sum;
}
float dot(vector<float> &x, vector<float> &y, float landval) {
  int i;
  float sum = 0.0;
  if (x.xpoints() != y.xpoints() ) {
    printf("cannot dot unequal sized vectors\n");
    return -1.;
  }
  for (i = 0; i < x.xpoints() ; i++) {
    if (x[i] != landval && y[i] != landval) {
      sum += x[i]*y[i];
    }
  }
  return sum;
}
void soiread(time_series<float> &soi) {
  float ori[12*120];
  int i=0, year = 0;
  char lines[900];
  FILE *fin;

  fin = fopen("bas.soi.dat","r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open the bas soi file\n");
    fflush(stdout);
    return ;
  }
    fgets(lines, 899, fin);
    fgets(lines, 899, fin);
    fgets(lines, 899, fin);
    fgets(lines, 899, fin);
// Page through to 1901
  while (year < 1901) {
    fgets(lines, 899, fin);
    sscanf(lines, "%d", &year);
    //printf("%s\n",lines);
  }

// Read in 1901 through 1990
  while (year < 1990) {  
    //printf("%s\n",lines);
    sscanf(lines, "%d %f %f %f %f %f %f %f %f %f %f %f %f",&year, &ori[i+0],
       &ori[i+1], &ori[i+2], &ori[i+3], &ori[i+4], &ori[i+5], &ori[i+6], 
       &ori[i+7], &ori[i+8], &ori[i+9], &ori[i+10], &ori[i+11] );
    i += 12;
    fgets(lines, 899, fin); 
  }

  for (i = 0; i < soi.xpoints() ; i++) {
    soi[i] = ori[i];
  }

  return;
} 
void flagger(metricgrid<float> &f, float nonval, float clobber) {
  //clobber is value to replace with nonval
  int i, count = 0;
  for (i = 0; i < f.xpoints() * f.ypoints() ; i++) {
     if (f[i] == clobber) { count += 1; f[i] = nonval; }
  }
}
  
void counter(metricgrid<int> &obscount, metricgrid<float> &conc, float nonval) {
  // update the observation count, adding one for each point that has a non-nonval
  int i;
  for (i = 0; i < conc.xpoints() * conc.ypoints(); i++) {
    if (conc[i] != nonval) obscount[i] += 1;
  }
}
void findland(metricgrid<float> &land, metricgrid<float> &conc, float landval) {
  int i;
  for (i = 0; i < land.xpoints()*land.ypoints(); i++) {
    if (conc[i] == landval) {
      land[i] = landval;
    }
    else {
      land[i] = 0.0;
    }
  }
}
void condsub(metricgrid<float> &orig, metricgrid<float> &avg, float landval) {
  int i;
  for (i =0; i < avg.xpoints() * avg.ypoints(); i++) {
    if (avg[i] != landval && orig[i] != landval ) { 
      orig[i] -= avg[i]; 
    }
    else {
      orig[i] = landval;
    }
  }
  return;
}

// Set up the conditional concentration color table
void condition_table(palette<unsigned char> &x) {
  int i, j;
  x.resize(14); 
  // i, then R, G, B
  for (i = 2; i < 11; i++) {
    j = (int) (0.5 + 255.0 * ((float) i)/ 11.0 );
    x.set_color(i, j, j, j);
  } 
  x.set_color(0, 0, 0, 0);        // Never iced is black
  x.set_color(1, 128, 0, 0);      // Minimal ice is dark red (more contrast)
  x.set_color(11, 255,   0, 0)  ; // Always iced is bright red
  x.set_color(12, 0,   255, 0)  ; // No data goes to green
  x.set_color(13, 0,     0, 255); // land is blue
}
// Define the color mapping function between elements and values:
// Must have already converted probabilities to percentages (0-100), with
//   land mask in y and undefined (224) in x.
template <class T>
T condition_color(T x, T land, int landval, int c2) {
  if (land == landval) {
    return 13;
  }
  else if (x == NO_DATA) {
    return 12;
  }
  else if (x == 0) {
    return 0;
  }
  else if (x == 100) {
    return 11;
  }
  else if (x > 100) {
    printf("Error, impossible concentration of %d\n", (int) x);
  }
  else {
    return (1 + x / 10);
  }
}

// This variant assumes the soi is being input and carries its own cutoff
//  as to what constitutes 'el nino' years.
template <class T>
float booltest(vector<T> &x, vector<T> &soi) {
   vector<bool> y(x.xpoints() ), soib(x.xpoints() );
   int i, nw = 0, mw = 0;

   if (x.rms() == 0.) {
     printf("Input series rms is zero!\n");
     return 0.0;
   }
   if (soi.rms() == 0.) {
     printf("Input soi rms is zero!\n");
     return 0.;
   }

   for (i = 0; i < x.xpoints(); i++) {
     if (x[i] > 0. && x[i] < 1.4) { y[i] = true; }
     else { y[i] = false; }
     if (soi[i] < -0.5) { soib[i] = true; }
     else { soib[i] = false; }

     if (y[i] == true  && soib[i] == false) nw += 1;
     if (y[i] == false && soib[i] == true ) mw += 1;
     //printf("%d %f %f %d %d\n",i, x[i], soi[i], y[i], soib[i]);
   }
   printf("nw, mw, prob = %4d %4d %6.4f  ",nw, mw, ((float)(nw*mw))
       / ((float)(x.xpoints() * (x.xpoints() - 1) ))  );

   return ((float)(nw*mw))  / ((float)(x.xpoints() * (x.xpoints() - 1) )) ;
} 

template <class T>
float r12(vector<T> &x, vector<T> &y, T mask) {
  float var1, var2;
  float r;
  
  var1 = dot(x, x, mask) / (x.xpoints() * x.complete(mask) );
  var2 = dot(y, y, mask) / (y.xpoints() * y.complete(mask) );

  r = dot(x, y, mask) / (x.xpoints() * x.complete(mask) );
  r /= sqrt( var1*var2);
  
  return r;
}
