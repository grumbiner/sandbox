#include <ctype.h>
#include <stdlib.h>
#include <math.h>
#include "ncepgrids.h"
#include "time_series.h"

// Program to work with Walsh data sets/climatology
// Robert Grumbine 15 July 1999.
// Note (ncepgrids.h) that Walsh naming of i/j are different
//  that RG method.
// Variant: Compute conditional concentrations and statistics
// Variant: Work with the binary (ice/no ice) information
// 30 July 1999

//Notes--------------------------------------------------------------
// Item to generalize is condsub (conditional subtraction, skipping 
//         operation where either value is a flag value) - put to grid_math

#define NMONTHS 1080
#define LAGMAX    73

void flagger(metricgrid<float> &f, float nonval, float clobber); 
                //clobber is value to replace with nonval
void counter(metricgrid<int> &obscount, metricgrid<float> &conc, float nonval);
extern void soiread(time_series<float> &soi, int firstyear);
extern void walshtobin(metricgrid<char> &c, metricgrid<int> &f) ;
extern float binary_continuous(mvector<int> &ice, mvector<float> &soi) ;
template <class T>
extern float bootstrap(mvector<T> &x, mvector<T> &y) ;


int main(void) {
  northgrid<float> nh;

  northwalsh<char> conc;
  northwalsh<int> numobs, obs_mon[12], fconc[NMONTHS];
  northwalsh<int> land;
  northwalsh<float> month[12], var, avg, correl, tmp;
  time_series<float> times(NMONTHS), soi(NMONTHS);
  mvector<int> time1(NMONTHS), time2(NMONTHS), time_mon[12];

  FILE *fin, *sourcein, *soiout;
  int nonval = 2; 
  int i;
  ijpt loc, loc2;
  latpt lloc1;
  palette<unsigned char> gg(19, 65);
  char fname[900];

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

  for(i = 0; i < 12; i++) {
    month[i].set(0.0);
    time_mon[i].resize(NMONTHS/12);
    obs_mon[i].set(0);
  }
  numobs.set(0);

  // Read in the data set
  i = 0;
  loc.i = 35;
  loc.j = 24;
  while (i < NMONTHS ) {
    conc.set('0');
    conc.walshread_c(fin);
    walshtobin(conc, fconc[i]);
    printf("Average %7.4f %6.3f\n", fconc[i].average(nonval), 
            fconc[i].operator[](loc) ); 
    i += 1;
  }
  fclose(fin);
  fflush(stdout);
  fflush(soiout);

  
// At this point, have the entire time series in one large(ish) array,
//   and a land mask is saved for reference.

// Compute the pointwise variances -- instead of variance, for a binary
//   series, use probability
  printf("About to try to compute the local probabilities\n"); fflush(stdout);
  for (loc.j = 0; loc.j < fconc[0].ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < fconc[0].xpoints(); loc.i++) {
     lloc1 = fconc[0].locate(loc);
     for (i = 0; i < time1.xpoints() ; i++) {
      time1[i] = fconc[i].operator[](loc);
     }
     var[loc] = time1.norm(1);
     if (var[loc] > 0.) {
       printf("Probability at %2d %2d, %7.2f %6.2f is %6.3f\n",
              loc.i, loc.j, lloc1.lon, lloc1.lat, var[loc]);
     }
  }
  }
  printf("Maximum probability is %f\n",var.gridmax());
  var *= 100.;
  var.xpm("var.xpm", 6, gg);
  fflush(stdout); 

// Now vary through the grid, taking only points which vary (p != 0 or 1)
// and check on the relation with SOI
  correl.set(1.0);
  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
    if (var[loc] == 0. || var[loc] >= 100.) continue;
    lloc1 = fconc[0].locate(loc);
    for (i = 0; i < time1.xpoints() ; i++) {
     time1[i] = fconc[i].operator[](loc);
    }
    correl[loc] = binary_continuous(time1, soi); 
       printf("soi delta prob at %2d %2d, %7.2f %6.2f is %5.2f %5.2f iceprob\n",
              loc.i, loc.j, lloc1.lon, lloc1.lat, correl[loc], var[loc]);
    
  }
  } // end of loc
  tmp.set(1.0);
  tmp -= correl; 
  tmp *= 100.;
  tmp.xpm("soicor.xpm", 6, gg);

//Now for pointwise through the grid, mvector vs. mvector:
  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
    correl.set(0.0); 
    if (var[loc] == 0. || var[loc] >= 100.) continue;
    for (i = 0; i < time1.xpoints() ; i++) {
     time1[i] = fconc[i].operator[](loc);
    }
    for (loc2.j = 0; loc2.j < land.ypoints(); loc2.j++) {
    for (loc2.i = 0; loc2.i < land.xpoints(); loc2.i++) {
      if (var[loc2] == 0. || var[loc2] >= 100.) continue;
      for (i = 0; i < time2.xpoints() ; i++) {
       time2[i] = fconc[i].operator[](loc2);
      }
      correl[loc2] = bootstrap(time1, time2);
      printf("cor %2d %2d to %2d %2d  %5.2f\n",loc.i, loc.j, 
                loc2.i, loc2.j, correl[loc2]  );
    }
    }
    sprintf(fname, "cor.%02d%02d.xpm",loc.i, loc.j);
    correl += 1.0;
    correl *= 100;
    correl.xpm(fname, 12, gg);
  }
  }

  return 0;

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
