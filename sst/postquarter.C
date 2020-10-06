#include "ncepgrids.h"
#define PERIOD 365.25
#define NPER    14

// Post-process the accumulated data grids to produce new mask
//   for a posteriori use, and provide some simple statistics/
// Robert Grumbine 

// 15 January 2009 Work with new (4-type) land masks

// Do:
//   global_quarter version
//   rearrange to avoid needing so much memory at one time (var3, skew, kurt, harmonics ...)
//   Numerical analysis issues for accumulating so many t^4 values

int main(int argc, char *argv[]) {
// Data to read in:
  global_quarter<int> count, ndays;
  global_quarter<float> high, low, dhigh, dlow;
  global_quarter<double> sx, sx2, sx3, sx4;
// Input mask (land, ocean, inland water):
  global_quarter<unsigned char> orimask;

// Harmonic analyses:
  #ifdef HARMONIC
  mvector<global_quarter<double> > ampl(NPER), phase(NPER);
  mvector<global_quarter<double> > sum_sin(NPER), sum_cos(NPER);
  #endif

// Output mask (land, ocean, inland water):
  global_quarter<unsigned char> outmask;

// Output diagnostics
  global_quarter<double> mean, var2, var3, var4, skew, kurt;
  mvector<int> hist(256);

// Local and flag values
  FILE *fin, *fout;
  ijpt loc, rloc, tloc;
  latpt ll;
  float tmp, offset = 273.15, scale = 100.;
  float tcrit = +2.00, tcrit2 = -9.99;
  double rm3 = 0, rm4 = 0;
  int tcount, range = 1;
  int i, n;
  unsigned char newland = 2, coast = 3, inland_water = 1, ocean = 0;
  unsigned char undefined = 224;

// Get input data
  fin = fopen(argv[1],"r");
    count.binin(fin);
    ndays.binin(fin);
    high.binin(fin);  high /= scale;
    low.binin(fin);   low  /= scale;
    dhigh.binin(fin); 
    dlow.binin(fin);
    sx.binin(fin);    sx  /= scale;
    sx2.binin(fin);   sx2 /= (scale*scale);
    sx3.binin(fin);   sx3 /= (scale*scale*scale);
    sx4.binin(fin);   sx4 /= (scale*scale*scale*scale);
    #ifdef HARMONIC
    for (i = 0; i < NPER; i++) {
      sum_sin[i].binin(fin);
      sum_cos[i].binin(fin);
    }
    #endif
  fclose(fin);

  fin = fopen(argv[2], "r");
  if ( fin == (FILE *) NULL) {
    printf ("Failed to open land mask file %s\n",argv[2]);
    return 1;
  }
  orimask.binin(fin);
  fclose(fin);

  fout = fopen(argv[3],"w");

  if (low.gridmin() != tcrit2) {
    printf("mismatch in gridmin and tcrit2 %f vs. %f delta = %e\n",
           low.gridmin(), tcrit2, tcrit2-low.gridmin() );
    tcrit2 = low.gridmin();
  }
  hist = 0;
  
// Now being big loop to scan over the previously processed
//   data and compute final statistics, do qc checks, and
//   start making a new mask file:
  for (loc.j = 0; loc.j < count.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < count.xpoints(); loc.i++) {
    ll = low.locate(loc);

    if (low[loc] <= tcrit2 && low[loc] != high[loc]) {
      printf("reynolds mask varies %3d %3d  %f %f  low %f high %f\n",loc.i, loc.j, 
           ll.lat, ll.lon, low[loc], high[loc]);
    }
    //if (ndays[loc] == 0) printf("always cold at %d %d  %f %f\n",loc.i, loc.j, 
    //       ll.lat, ll.lon);

    if (high[loc] <= tcrit2 && (orimask[loc] == ocean || orimask[loc] == inland_water
                              || orimask[loc] == coast ) ) {
      // mismatch, ice thinks it's water, Reynolds thinks it's land 
      //printf("mismatch at %4d %4d  %7.3f %7.3f  ",loc.i, loc.j, ll.lat, ll.lon);

      tmp = 999.;
      tcount = 0;
      for (tloc.j = -range; tloc.j <= range; tloc.j++) {
        rloc.j = loc.j + tloc.j;
      for (tloc.i = -range; tloc.i <= range; tloc.i++) {
        rloc.i = loc.i + tloc.i;
        if (low[rloc] > tcrit2) {
          if (low[rloc] < tmp) {
            tmp = low[rloc];
            tcount += 1; 
          }
        }
      }
      }
      if (tcount > 0) {  // then we managed to find a fix in range
        low[loc] = tmp;
        high[loc] = tmp;
        //printf("fixed with value of %f count %d\n",tmp, tcount );
      }
      else {
        //printf(" -- no fix.  Orig = %d\n",orimask[loc]);
      }
    } // end of trying to fix land mask/tmin between ice and reynolds masks


// Now build our revised mask file:
    if (low[loc] > tcrit2 && orimask[loc] == newland ) {
      printf("Reynolds water %6.2f on RG land %7.3f %7.3f\n",
            low[loc], ll.lat, ll.lon);
    } 

// Land that Reynolds agrees is land:
    if (orimask[loc] == newland && low[loc] == tcrit2 ) {
      outmask[loc] = orimask[loc];
    }
// Ocean
    else if (orimask[loc] == ocean ) {
      if (low[loc] >= + 26.0) {  // Hurricane magic SST
        outmask[loc] = 158;
      }
      else if (low[loc] >= + 24.0) {
        outmask[loc] = 159;
      }
      else if (low[loc] >= + 22.0) {
        outmask[loc] = 160;
      }
      else if (low[loc] >= + 19.0) {
        outmask[loc] = 161;
      }
      else if (low[loc] >= + 15.0) {
        outmask[loc] = 162;
      }
      else if (low[loc] >= +  9.0) {
        outmask[loc] = 163;
      }
      else if (low[loc] >= +  2.15) {
        outmask[loc] = 164;
      }
      else if (low[loc] >= +  -3.0) {
        outmask[loc] = 165;
      }
      else {
        printf("no valid tmin for %7.3f %7.3f orimask = %3d\n",ll.lat, ll.lon, orimask[loc]);
        outmask[loc] = undefined;
      }
    }
// Inland water
    else if (orimask[loc] == inland_water ) {
      if (low[loc] >= + 7.0) {
        outmask[loc] = 170;
      }
      else if (low[loc] >= +  4.0) {
        outmask[loc] = 171;
      }
      else if (low[loc] >= +  2.15) {
        outmask[loc] = 172;
      }
      else if (low[loc] >  +  0.0) { //Note strictly greater, fresh water
        outmask[loc] = 173;
      }
      else if (low[loc] >= +  -3.0) {
        outmask[loc] = 174;
      }
      else {
        printf("no valid tmin for %7.3f %7.3f orimask = %3d\n",ll.lat, ll.lon, orimask[loc]);
        outmask[loc] = undefined;
      }
    }
  else if (orimask[loc] == newland) {
    // Point that RG has as land but Reynolds gives sst for, treat as ocean:
      if (low[loc] >= + 26.0) {  // Hurricane magic SST
        outmask[loc] = 158;
      }
      else if (low[loc] >= + 24.0) {
        outmask[loc] = 159;
      }
      else if (low[loc] >= + 22.0) {
        outmask[loc] = 160;
      }
      else if (low[loc] >= + 19.0) {
        outmask[loc] = 161;
      }
      else if (low[loc] >= + 15.0) {
        outmask[loc] = 162;
      }
      else if (low[loc] >= +  9.0) {
        outmask[loc] = 163;
      }
      else if (low[loc] >= +  2.15) {
        outmask[loc] = 164;
      }
      else if (low[loc] >= +  -3.0) {
        outmask[loc] = 165;
      }
      else {
        printf("no valid tmin for %7.3f %7.3f orimask = %3d\n",
                   ll.lat, ll .lon, orimask[loc]);
        outmask[loc] = undefined;
      }
    }
// Treat coast as fresh water:
  else if (orimask[loc] == coast) {
    // Point that RG has as land but Reynolds gives sst for, treat as ocean:
      if (low[loc] >= + 26.0) {  // Hurricane magic SST
        outmask[loc] = 158;
      }
      else if (low[loc] >= + 24.0) {
        outmask[loc] = 159;
      }
      else if (low[loc] >= + 22.0) {
        outmask[loc] = 160;
      }
      else if (low[loc] >= + 19.0) {
        outmask[loc] = 161;
      }
      else if (low[loc] >= + 15.0) {
        outmask[loc] = 162;
      }
      else if (low[loc] >= +  9.0) {
        outmask[loc] = 163;
      }
      else if (low[loc] >= +  2.15) {
        outmask[loc] = 164;
      }
      else if (low[loc] >= +  -3.0) {
        outmask[loc] = 165;
      }
      else {
        printf("no valid tmin for %7.3f %7.3f orimask = %3d\n",
                     ll.lat, ll.lon, orimask[loc]);
        outmask[loc] = undefined;
      }
    }
    else {
      printf("fatality at %4d %4d, %7.3f %7.3f should not get here %3d %3d\n",
            loc.i, loc.j, ll.lat, ll.lon, orimask[loc], outmask[loc]);
    }
  

// Compute some higher order statistics:
    if (count[loc] != 0) {
      n = count[loc];
      mean[loc] = sx[loc] / (float)count[loc];
      var2[loc] = (sx2[loc] - n*mean[loc]*mean[loc]) / n;
      var3[loc] = (sx3[loc] - 3*mean[loc]*sx2[loc] + 3*mean[loc]*mean[loc]*sx[loc] 
                       - n*mean[loc]*mean[loc]*mean[loc]) / n ;
      var4[loc] = (sx4[loc] - 4*mean[loc]*sx3[loc] + 6*mean[loc]*mean[loc]*sx2[loc] 
                       - 4*mean[loc]*mean[loc]*mean[loc]*sx[loc] 
                       + n*mean[loc]*mean[loc]*mean[loc]*mean[loc] ) / n ;
      rm4 = max(rm4, sx4[loc]/n);
      rm3 = max(rm3, sx3[loc]/n);
      if (var2[loc] > 1.e-2) {
        skew[loc] = var3[loc] / pow(fabs(var2[loc]),1.5);
        kurt[loc] = var4[loc] / (var2[loc]*var2[loc])     - 3.0;
      }
      else {
        skew[loc] = 0.;
        kurt[loc] = 0.;
      }
    }
 
// Compute fourier amplitude and phase:
    #ifdef HARMONIC
    for (i = 0; i < NPER; i++) {
       ampl[loc][i] = sqrt(sum_sin[loc][i]*sum_sin[loc][i] + 
                      sum_cos[loc][i]*sum_cos[loc][i]    );
       phase[loc][i] = atan2(sum_sin[loc][i], sum_cos[loc][i] );
    }
    #endif

  }
  }

////////////////////////////////////////////////////////////////////////
////  Done with straightforward pass through the processed file 
////////////////////////////////////////////////////////////////////////

// Now do fussier pass(es) to try to infer proper mask values for the
//   so-far unfillable points
  int iter = 0, fix = 5;
  while (iter < 5 && fix > 0) {
    fix = 0;
    for (loc.j = 0; loc.j < outmask.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < outmask.xpoints(); loc.i++) {
      if (outmask[loc] == undefined) {
        ll = outmask.locate(loc);
        tmp = 999.;
        tcount = 0;
        for (tloc.j = -range; tloc.j <= range; tloc.j++) {
          rloc.j = loc.j + tloc.j;
        for (tloc.i = -range; tloc.i <= range; tloc.i++) {
          rloc.i = loc.i + tloc.i;
          // Note that this isn't extremely clean as the 160 and 170 
          //    series are not commensurate
          if (outmask[rloc] > newland && outmask[rloc] < undefined) {
            if (outmask[rloc] < tmp) {
              tmp = outmask[rloc];
              tcount += 1;
            }
          }
        }
        }
        if (tcount > 0) {  // then we managed to find a fix in range
          fix++;
          outmask[loc] = (unsigned char) tmp;
          printf("iter %2d latefix with %f count %1d at %7.3f %7.3f\n",
                 iter, (float) outmask[loc], tcount, ll.lat, ll.lon );
        }
      }
    }
    }
    iter++;
    range++;
  }

  for (loc.j = 0; loc.j < outmask.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < outmask.xpoints(); loc.i++) {
    hist[outmask[loc] ] += 1;
    if (outmask[loc] == undefined) {
      ll = outmask.locate(loc);
      printf("no way to fill %4d %4d %7.3f %7.3f\n",loc.i, loc.j, ll.lat, ll.lon);
    }
  }
  }

////////////////////////////////////////////////////////////////////////
// Finally, make output files and compute some integrals

  outmask.binout(fout);
  low.binout(fout);
  high.binout(fout);

  // convert to Kelvin if needed
  //if (mean.gridmax() < offset) mean += offset;
  mean.binout(fout);
  var2.binout(fout);
  var3.binout(fout);
  var4.binout(fout);
  printf("mean max min avg %6.2f %6.2f %6.2f\n",
                      mean.gridmax(tcrit2), mean.gridmin(tcrit2), mean.average(tcrit2));
  printf("low  max min avg %6.2f %6.2f %6.2f\n",
                      low.gridmax(tcrit2), low.gridmin(tcrit2), low.average(tcrit2));
  printf("high max min avg %6.2f %6.2f %6.2f\n",
                      high.gridmax(tcrit2), high.gridmin(tcrit2), high.average(tcrit2));
  printf("var2 max min avg %f %f %f\n",var2.gridmax(), var2.gridmin(), var2.average());
  printf("var3 max min avg %f %f %f\n",var3.gridmax(), var3.gridmin(), var3.average());
  printf("var4 max min avg %f %f %f\n",var4.gridmax(), var4.gridmin(), var4.average());
  printf("skew max min avg %f %f %f\n",skew.gridmax(), skew.gridmin(), skew.average());
  printf("kurt max min avg %f %f %f\n",kurt.gridmax(), kurt.gridmin(), kurt.average());
  printf("rm4 = %f, rm3 = %f\n",(float) pow(rm4, 1./4.), (float) pow(rm3, 1./3.) );
//Kurtosis = (in my notation): (var4 / var2^2)
//            3 = kurtosis of normal curve, already subtracted
//            Negative -> more peaked than normal curve
//Skew = (in my notation) var3 / (var2^1.5)

  global_quarter<float> flag;
  double sumarea = 0.0;
  for (i = 0; i < 255; i++) {
    if (hist[i] != 0 ) {
      printf("histogram %3d  %7d ",i,hist[i]);
      flag.set((float)0.);
      for (loc.j = 0; loc.j < flag.ypoints(); loc.j++) {
      for (loc.i = 0; loc.i < flag.xpoints(); loc.i++) {
        if (outmask[loc] == i) flag[loc] = 1.0;
      }
      }
      printf(" area %9.5f\n",flag.integrate()/1.e12);
      sumarea += flag.integrate()/1.e12;
    }
  }

// Integral test
  #ifdef VERBOSE
  flag.set((float) 1.0);
  printf("total flagged area = %f, total globe area = %f\n",
               sumarea,  flag.integrate()/1.e12);
  #endif


  #ifdef HARMONIC
  for (i = 0; i < NPER; i++) {
    ampl[i].binout(fout);
    printf("amp %2d max min avg %f %f %f\n",i, ampl[i].gridmax(), ampl[i].gridmin(), ampl[i].average());
    phase[i].binout(fout);
  }  
  #endif

  fclose(fin);

  return 0;
  // add graphic of some sort here
  palette<unsigned char> gg(19, 65);
  var2.scale();
  var2.xpm("var2.xpm",7,gg);

  skew.scale();
  skew.xpm("skew.xpm",7,gg);

  kurt.printer(stdout);
  kurt.scale();
  kurt.xpm("kurt.xpm",7,gg);

  return 0;
}
