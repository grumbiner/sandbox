#include <ctype.h>
#include <stdlib.h>
#include "ncepgrids.h"

// Program to work with Walsh data sets/climatology
// Robert Grumbine 15 July 1999.
// Note (ncepgrids.h) that Walsh naming of i/j are different
//  that RG method.

#define NMONTHS 1080

void walshtof(metricgrid<char> &c, metricgrid<float> &f) ;
void getvec(ijpt &loc, vector<float> &x, metricgrid<float> *ary);
float dot(vector<float> &x, vector<float> &y) ;

int main(void) {
  FILE *fin;
  northwalsh<char> conc;
  northwalsh<float> land, landold, tconc, fconc[NMONTHS];
  northwalsh<float> month[12], var, avg;
  northgrid<float> nh;
  vector<float> time1(NMONTHS), time2(NMONTHS); 
  float landmask = 1.57, rij;
  ijpt loc, loc2;
  int i;
  palette<unsigned char> gg(19, 65);

// Start reading in concentration field:
  fin = fopen("arctic-concentration-01-90.dat","r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open the arctic concentration file\n");
    return 1;
  }

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
    //printf("Average %8.4f %f\n", fconc[i].average(landmask), 
    //        fconc[i].operator[](loc) ); 
    month[ (i%12) ] += fconc[i];
    i += 1;
  }
  fclose(fin);
// At this point, have the entire time series in one large(ish) array


// Find the monthly averages, ignoring months of nodata:
  for (i = 0; i < 12; i++) {
    month[i] /= (NMONTHS / 12);
    //Note that this is required because the averaging process of 
    //  floating point numbers is not exact.
    landmask = month[i].gridmax();
    printf("Average for month %d is %f\n", i, month[i].average(landmask) );
  }

// Remove the average:
  printf("About to try to remove the monthly averages\n"); fflush(stdout);
  for (i = 0; i < NMONTHS; i++) {
    fconc[i] -= month[ i%12 ] ;
    printf("Demeaned average for month %d is %f\n", i, 
                                          fconc[i].average(landmask) );
  }

  printf("About to try to compute the local variances\n"); fflush(stdout);
  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
     for (i = 0; i < time1.xpoints() ; i++) {
      time1[i] = fconc[i].operator[](loc);
     }

     var[loc] = dot(time1, time1)/ time1.xpoints();
  }
  }
  land = var;
  land.scale();
  land.xpm("var.xpm", 7, gg);


// Now have figures in hand to start computing the cross-covariances/
//  correlations
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

// Now vary through the grid, taking only non-land points which don't have
//   zero variance (would lead to undefineds).  (This cuts out 3/4ths of the
//   points).
  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
    if (land[loc] > 1.0 || var[loc] == 0.) continue;

    for (i = 0; i < time1.xpoints() ; i++) {
     time1[i] = fconc[i].operator[](loc);
    }
    // Note that we're taking advantage of Rij = Rji, Rii = 1.0;
    for (loc2.j = loc.j; loc2.j < land.ypoints(); loc2.j++) {
    for (loc2.i = loc.i; loc2.i < land.xpoints(); loc2.i++) {
      if (land[loc2] > 1.0 || var[loc2] == 0.) continue;
      if (loc2 == loc) continue;
      for (i = 0; i < time2.xpoints() ; i++) {
       time2[i] = fconc[i].operator[](loc2);
      }
      rij = dot(time1, time2)/( (float)NMONTHS);
      rij /= sqrt(var[loc]*var[loc2]);
      if ( (rij*rij) > 0.02*0.02) { 
        printf("correl %6.3f  %2d %2d to %2d %2d \n", rij, loc.i, loc.j, 
                        loc2.i, loc2.j); 
      }
    }
    } // end of loc2
  }
  } // end of loc

      



// Print out monthly mean fields
  for (i = 0; i < 12; i++) {
   char fname[900];
   sprintf(fname, "nh.%02d.xpm", i);
   month[i] *= 100;
   nh.fromall(month[i], land, land.gridmax(), 0.);
   nh.xpm(fname, 7, gg);
  }
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
      f[loc] = 1.57; // Land tag in RG systems
    }
    else {
      printf("Unknown character type %2d %2d %1c\n",loc.i, loc.j, c[loc]);
    }
  }
  }

  return;
}
void getvec(ijpt &loc, vector<float> &x, metricgrid<float> *ary) {
  int i;
  for (i = 0; i < x.xpoints() ; i++) {
    printf("getvec, i, loc %d  %3d %3d\n",i, loc.i, loc.j); fflush(stdout);
    x[i] = ary[i].operator[](loc);
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
