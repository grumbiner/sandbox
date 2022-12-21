#include <ctype.h>
#include <stdlib.h>
#include "ncepgrids.h"
#include "time_series.h"

// Program to work with Walsh data sets/climatology
// Robert Grumbine 15 July 1999.
// Note (ncepgrids.h) that Walsh naming of i/j are different
//  that RG method.
// Variant: Compute conditional concentrations and statistics
// Subroutines extracted from main program for common use 30 November 1999
// 10 April 2000

void walshctof(metricgrid<char> &c, metricgrid<float> &f) ;
void walshitof(metricgrid<int> &c, metricgrid<float> &f) ;
void soiread(time_series<float> &soi, int firstyear);
void flagger(metricgrid<float> &f, float nonval, float clobber);
                //clobber is value to replace with nonval
void counter(metricgrid<int> &obscount, metricgrid<float> &conc, float nonval);

void condition_table(palette<unsigned char> &x) ;
template <class T>
T condition_color(T x, T y, int c1, int c2) ;
template <class T>
T ice_coloring(T conc, T sland, int cres, int cbase) ;



void walshitof(metricgrid<int> &c, metricgrid<float> &f) {
// Translate out of walsh character encoding to floating point concentrations
  ijpt loc;

  for (loc.j = 0; loc.j < c.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < c.xpoints() ; loc.i++) {
     f[loc] = c[loc]/10.;
  }
  }

  return;
}

void walshctof(metricgrid<char> &c, metricgrid<float> &f) {
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
void soiread(time_series<float> &soi, int firstyear) {
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
// Page through to start
  while (year < firstyear) {
    fgets(lines, 899, fin);
    sscanf(lines, "%d", &year);
    //printf("%s\n",lines);
  }

// Read in firstyear through 1990
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
  return 12;
}
template <class T>
T ice_coloring(T conc, T sland, int cres, int cbase) {
   if ((int) (0.5 + sland) == (int) 150) {
     return 0;
   }
   else if ((int) (0.5 + sland) == (int) COAST) {
     return 1;
   }
   else if (conc == NO_DATA || conc == BAD_DATA) {
     return 2;
   }
   else if (conc == WEATHER) {
     return 3;
   }
   else if (conc <= MIN_CONC) {
     return 4;
   }
   else {
     return ( 4 + (min((T)104, max((T)0, (T)conc) ) - MIN_CONC)/cres ) ;
   }

   return 0;
}


template <class T>
float booltest(mvector<T> &x, mvector<T> &soi) ;

//Correlation for when neither point is masked
template <class T>
float r12(mvector<T> &x, mvector<T> &y, T mask) ;

//For mvector/time series operations:
template <class T>
void swap (mvector<time_series<T> > &regress, mvector<ijpt> &points,
            int open, int imax) ;
template<class T>
void orthog(time_series<T> &x, time_series<T> &y, T flag) ;


//////////////////
template<class T>
void orthog(time_series<T> &x, time_series<T> &y, T flag) {
   T prod;
   time_series<T> tmp(y.xpoints());
   int i;

   prod = dot(x, y, flag);
   for (i = 0; i < x.xpoints() ; i++) {
      if (x[i] != flag && y[i] != flag) {
        y[i] -= prod*x[i];
      }
   }
   y.normalize(flag);
   return;
}

template <class T>
void swap (mvector<time_series<T> > &regress, mvector<ijpt> &points,
            int open, int imax) {
  ijpt tij;
  time_series<T> tser(regress[0].xpoints() );

  tij  = points[open];
  tser = regress[open];
  points[open]  = points[imax];
  regress[open] = regress[imax];
  points[imax]  = tij;
  regress[imax] = tser;
  return;
}

// This variant assumes the soi is being input and carries its own cutoff
//  as to what constitutes 'el nino' years.
template <class T>
float booltest(mvector<T> &x, mvector<T> &soi) {
   mvector<bool> y(x.xpoints() ), soib(x.xpoints() );
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
float r12(mvector<T> &x, mvector<T> &y, T mask) {
  float var1, var2;
  float r;

  var1 = dot(x, x, mask) / (x.xpoints() * x.complete(mask) );
  var2 = dot(y, y, mask) / (y.xpoints() * y.complete(mask) );

  r = dot(x, y, mask) / (x.xpoints() * x.complete(mask) );
  r /= sqrt( var1*var2);

  return r;
}

float dot(mvector<float> &x, mvector<float> &y) ;
float dot(mvector<float> &x, mvector<float> &y, float landval) ;

float dot(mvector<float> &x, mvector<float> &y) {
  int i;
  float sum = 0.0;
  if (x.xpoints() != y.xpoints() ) {
    printf("cannot dot unequal sized mvectors\n");
    return -1.;
  }
  for (i = 0; i < x.xpoints() ; i++) {
    sum += x[i]*y[i];
  }
  return sum;
}
float dot(mvector<float> &x, mvector<float> &y, float landval) {
  int i;
  float sum = 0.0;
  if (x.xpoints() != y.xpoints() ) {
    printf("cannot dot unequal sized mvectors\n");
    return -1.;
  }
  for (i = 0; i < x.xpoints() ; i++) {
    if (x[i] != landval && y[i] != landval) {
      sum += x[i]*y[i];
    }
  }
  return sum;
}

void walshtof(metricgrid<int> &c, metricgrid<float> &f, float nonval) ;
void walshtof(metricgrid<int> &c, metricgrid<float> &f, float nonval) {
// Translate out of walsh character encoding to floating point concentrations
  ijpt loc;

  for (loc.j = 0; loc.j < c.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < c.xpoints() ; loc.i++) {
     f[loc] = c[loc]/10.;
     if (f[loc] > 1.1) f[loc] = nonval;
  }
  }

  return;
}


//Compare two binary vectors for independance
float independ(mvector<int> &x, mvector<int> &y) ;

float independ(mvector<int> &x, mvector<int> &y) {
  int i;
  mvector<int> z(x.xpoints() );
  float px, py, pz;
  
  if (x.xpoints() == 0) return 0; 
  z = y;
  z *= x;
  px = x.norm(1);
  py = y.norm(1);
  pz = z.norm(1);
  
  // If the mvectors are independant, then p(xy) = p(x) * p(y);
  // where we're letting z = xy.  Note that the norm(1) simply sums the
  // elements (i.e., a counting for binary series), and z = x*y is
  // the same for binary series as z = x AND y;
  if (pz != 0.) { 
    return (pz - px*py)*x.xpoints() / sqrt(x.xpoints() * pz*(1.-pz) );
  }
  else if (px != 0.) {
    return (pz - px*py)*x.xpoints() / sqrt(x.xpoints() * px*(1.-px) );
  }
  else if (py != 0.) {
    return (pz - px*py)*x.xpoints() / sqrt(x.xpoints() * py*(1.-py) );
  }
  else {
    return 0;
  }

  return 0;
}

float information(mvector<int> &x) ;
float information(mvector<int> &x, int cats) ;

// Compute the Shannon information of a (presumed) binary series
float information(mvector<int> &x) {
  int i, found=0;
  float p, info;
  for (i = 0; i < x.xpoints(); i++) {
    if (x[i] != 0) found += 1;
  }
  p = (float) found / (float) x.xpoints() ;
  if (p == 0. || p == 1.) return 0.;
  info  = -   p    * log10(p)    / log10(2.);
  info += - (1.-p) * log10(1.-p) / log10(2.);
  return info;
}
// Compute info for category ranges 0 - cats
float information(mvector<int> &x, int cats) {
  int i;
  mvector<int> found(cats);
  float p, info;
  for (i = 0; i < x.xpoints(); i++) {
    found[ x[i] ] += 1;
  }
  info = 0.;
  for (i = 0; i < cats; i++) {
    if (found[i] != 0) {
      p = (float) found[i] / (float) x.xpoints() ;
      info +=  -  p * log10(p) / log10(2.);
    }
  }
  return info;
}

template <class T> 
void shuffle(mvector<T> &source, mvector<T> &dest, int seed) ;
template <class T> 
float bootstrap(mvector<T> &x, mvector<T> &y) ;

//Shuffle a mvector, returning result in another mvector
template <class T>
void shuffle(mvector<T> &source, mvector<T> &dest, int seed) {
  int nrem = source.xpoints();
  int i, count=0;
  mvector<T> orig(source.xpoints() );
  
  //Transfer source data to a temporary mvector which we will
  // modify
  orig = source;

  //Initialize the pseudorandom number generator
  srand(seed);
  while (nrem > 0) {
    i = (int) ((float)nrem * rand() / (RAND_MAX + 1.0) );
    dest[count] = orig[i];
    orig[i] = orig[nrem-1]; //Bring the end of series value down to 'i'
    nrem  -= 1;
    count += 1;
  } 
  
  return;
}

//Using the above 'shuffle', take two mvectors find their correlation,
//  and then find the % of the time that a reshuffling beats that
//  correlation.  Assume that the mean has been removed already.
template <class T>
float bootstrap(mvector<T> &x, mvector<T> &y) {
  float sd1, sd2, corref;
  int i, count = 0, maxcount = 1000 ;
  mvector<T> z(x.xpoints() );
  
  sd1 = x.norm(2);
  sd2 = y.norm(2);
  if (sd1*sd2 == 0.) return -1.; 
  corref = fabs(dot(x, y) / (sd1*sd2));
  for (i = 0; i < maxcount; i++) {
    shuffle(y, z, i); //last is seed, which must vary between calls
    if (fabs(dot(x,z) / (sd1*sd2) ) >= corref) {
      count += 1;
    }
  }
  
  return (float) count / (float) maxcount;
}

//30 July 1999
#include "metric.h"

void condsub(metricgrid<float> &orig, metricgrid<float> &avg, float landval) ;

//Conditional subtraction -- skip points where either is masked and use
//                           mask as result
void condsub(metricgrid<float> &orig, metricgrid<float> &avg, float mask) {
  int i;
  for (i =0; i < avg.xpoints() * avg.ypoints(); i++) {
    if (avg[i] != mask && orig[i] != mask ) { 
      orig[i] -= avg[i]; 
    }
    else {
      orig[i] = mask;
    }
  }
  return;
}

void enmask(vector<float> &x, float maskval) ;
void demean(vector<float> &x, float maskval);
void specprint(vector<float> &re, vector<float> &im, FILE *fout) ;

void enmask(vector<float> &x, float maskval) {
  int i;
  for (i = 0; i < x.xpoints(); i++) {
    if (x[i] > 1.28 ) x.vec[i] = maskval;
  }
  return;
}

void demean(vector<float> &x, float maskval) {
  int i;
  float mean;

  mean = x.average(maskval);
  for (i = 0; i < x.xpoints() ; i++) {
    if (x.vec[i] != maskval) x.vec[i] -= mean;
  }
  return;
}
void specprint(vector<float> &re, vector<float> &im, FILE *fout) {
  int i;
  fprintf(fout, "%8.3f %7.5f %8.3f \n",(float) 2*re.xpoints(), 0.0,
               re.vec[0]*re.vec[0] + im.vec[0]*im.vec[0]);

  for (i = 1; i < re.xpoints() / 2 + 1; i++) {
    fprintf(fout, "%8.3f %7.5f %8.3f \n",(float) re.xpoints()/(float) i, 
                   (float) i / (float) re.xpoints(),
                   re.vec[i]*re.vec[i] + im.vec[i]*im.vec[i]);
  }
  return;
} 
  
