#include "points.h"
#include "ncepgrids.h"

// Find slope + intercept + harmonics on squared residuals -> variance climatology
// 10 April 2013

#define PERIOD 365.259636
#define NPER 6

#define TROPICAL    365.24219
#define SIDEREAL    365.256363
#define ANOMALISTIC 365.259635

#define lunar_sidereal 27.32166
#define lunar_perigee (8.85*SIDEREAL)
#define lunar_node    (18.6*SIDEREAL)

////////////////////////////////////////////////////////////////////////
void compute_harmonics(mvector<global_quarter<long double> > &sum_sin, mvector<global_quarter<long double> > &sum_cos, mvector<global_quarter<float> > &ampl, mvector<global_quarter<float> > &phase, int npts, mvector<double> &period) ;

template <class T>
void compute_trends(grid2<T> &sx, grid2<T> &sx2, grid2<T> &sxy, grid2<T> &sy, grid2<T> &sy2, grid2<float> &slope, grid2<float> &intercept, grid2<float> &correl, grid2<float> &tstatistic, int npts) ;

////////////////////////////////////////////////////////////////////////

int main(int argc, char *argv[]) {
// For local reading and accumulating
  global_quarter<float> tmp;
  global_quarter<long double> stmp, stmp2, sumy, sumy2;
// For local trend
  global_quarter<long double> sumx, sumx2, sumxy;  
// For harmonics
  mvector<global_quarter<long double> > sum_sin(NPER), sum_cos(NPER);
  long double fsin[NPER], fcos[NPER];
  mvector<double> omega(NPER), period(NPER);

// utility:
  FILE *fin;
  int i, j, npts, nharm = 6;
  ijpt loc;
  float time;
  global_quarter<float> expect;

///////////////////////////////////////////////////////////
// Start working
  npts = argc - 2;
  printf("npts = %d\n",npts); fflush(stdout);

  //fin = fopen(argv[1],"r");
  //read_first_pass(fin, slope, intercept);
  //fclose(fin);

// Initialize:
  sumy.set((long double) 0);
  sumy2.set((long double) 0);
  sumx.set((long double) 0);
  sumxy.set((long double) 0);
  sumx2.set((long double) 0);
  for (i = 0; i < NPER; i++) {
    sum_sin[i].set((double) 0);
    sum_cos[i].set((double) 0);
  }

  fin = fopen("doodson","r");
  if (fin == (FILE*) NULL) {
    printf("failed to open doodson\n"); fflush(stdout);
    return 1;
  }
  int d1, d2, d3, d4, d5, d6;
  for (j = 0; j < NPER; j++) {
    fscanf(fin, "%d %d %d %d %d %d\n",&d1, &d2, &d3, &d4, &d5, &d6);
    printf("%d %d %d %d %d %d  ",d1, d2, d3, d4, d5, d6);
    omega[j] = 
        1/(TROPICAL)*d1        +
        1/(SIDEREAL)*d2        + 
        1/(ANOMALISTIC)*d3     + 
        1/(lunar_sidereal)*d4  + 
        1/(lunar_node)*d5      + 
        1/(lunar_perigee)*d6 ;
     period[j] = 1./omega[j];
     omega[j] *= 2.*M_PI;
     printf(" %e %e\n",(double) period[j],(double) omega[j]);
  }
  fflush(stdout);

////////////////////////////////////////////////////////////////////////////
// Start scan through observations:
  for (i = 2; i < argc; i++) {
    time = (float) (i - 2);
    fin = fopen(argv[i], "r");
    tmp.binin(fin);
    fclose(fin);
    tmp /= 100.; // convert to degrees
    printf("read in file %d max = %e\n",i, tmp.gridmax() ); fflush(stdout);

    conv(tmp, stmp);
    stmp *= stmp;

    // for harmonics
    for (j = 0; j < NPER; j++) {
      fsin[j] = sin(omega[j]*(time) ) * 2./period[j];
      fcos[j] = cos(omega[j]*(time) ) * 2./period[j];
    }

    for (int index = 0; index < sumy.xpoints()*sumy.ypoints(); index++) {
      // for trends:
      sumy[index]  += stmp[index];
      sumy2[index] += stmp[index]*stmp[index];
      sumx[index]  += (long double) (time);
      sumx2[index] += (long double) (time*time);
      sumxy[index] += stmp[index]*(long double) (time);

      // for harmonics
      for (j = 0; j < NPER; j++) {
        sum_sin[j][index] += stmp[index]*fsin[j];
        sum_cos[j][index] += stmp[index]*fcos[j];
      }
    }

  }
// Done doing accumulations
////////////////////////////////////////////////////////////////////////////

// Find things of interest and write them out:
  FILE *fout;
  global_quarter<float> slope, intercept, correl, tstatistic;
  compute_trends(sumx, sumx2, sumxy, sumy, sumy2, slope, intercept, correl, tstatistic, npts);

  printf("slope max min avg rms %f %f %f %f\n",slope.gridmax(), slope.gridmin(), slope.average(), slope.rms() );
  printf("intercept max min avg rms %f %f %f %f\n",intercept.gridmax(), intercept.gridmin(), intercept.average(), intercept.rms() );
  fflush(stdout);

// Defer this declaration for memory considerations
  fout = fopen("var_dood_harmonics_out", "w");
  mvector<global_quarter<float> > ampl(NPER), phase(NPER);
  global_quarter<long double> tvar;
  global_quarter<float> var;

  compute_harmonics(sum_sin, sum_cos, ampl, phase, npts, period);

  // var is variance after removal of trend but before removal of harmonics
  for (int index = 0; index < var.xpoints()*var.ypoints(); index++) {
    tvar[index] = (sumy2[index] - sumy[index]*sumy[index] / npts );
    var[index] = (float) ((double) tvar[index] / (double) (npts*100*100));
  }
  var.binout(fout);
  printf("var stats: %f %f %f %f\n",var.gridmax(), var.gridmin(), var.average(), var.rms() );
  fflush(stdout);

  for (j = 0; j < NPER; j++) {
    printf("period %7.2f  ampl stats %6.3f %5.3f %5.3f\n",(double)period[j], ampl[j].gridmax()/1, ampl[j].average()/1, ampl[j].rms()/1 );
    fflush(stdout);
    ampl[j].binout(fout);  fflush(fout);
    phase[j].binout(fout); fflush(fout);
  }
  
  fclose(fout);


  fout = fopen("var_reference_fields","w");
  intercept.binout(fout);
  slope.binout(fout);
  for (j = 0; j < nharm; j++) {
    ampl[j].binout(fout);
    phase[j].binout(fout);
  }
  fclose(fout);
//////////////////////////////////////////////////////////////////////


  return 0;
}


////////////////////
void compute_harmonics(mvector<global_quarter<long double> > &sum_sin, mvector<global_quarter<long double> > &sum_cos, mvector<global_quarter<float> > &ampl, mvector<global_quarter<float> > &phase, int npts, mvector<double> &period) {
  ijpt loc;
  int i;
// Compute fourier amplitude and phase:
  for (loc.j = 0; loc.j < ampl[0].ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ampl[0].xpoints(); loc.i++) {
    for (i = 0; i < NPER; i++) {
       ampl[i][loc] = sqrt(sum_sin[i][loc]*sum_sin[i][loc] +
                           sum_cos[i][loc]*sum_cos[i][loc]    ) / ((float) npts / (float) period[i]);
       phase[i][loc] = atan2(sum_sin[i][loc], sum_cos[i][loc] );
    }
  }
  }

  return;
}
////////////////////
template <class T>
void compute_trends(grid2<T> &sx, grid2<T> &sx2, grid2<T> &sxy, grid2<T> &sy, grid2<T> &sy2, grid2<float> &slope, grid2<float> &intercept, grid2<float> &correl, grid2<float> &tstatistic, int npts) {
  ijpt loc;

// x = time, y = sst.
  for (loc.j = 0; loc.j < sx.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < sx.xpoints(); loc.i++) {
    slope[loc] = (float) ((double) ((double) npts*sxy[loc] - (double) sx[loc]*sy[loc]) /
                          (double) ((double) npts*sx2[loc] - (double) sx[loc]*sx[loc])  ) ;
    intercept[loc] = ((double) sy[loc]/npts - slope[loc]*(double) sx[loc]/(double) npts);
    correl[loc] =    ((double) npts*sxy[loc] - (double) sx[loc]*sy[loc]) /
                 sqrt((double) npts*sx2[loc] - (double) sx[loc]*sx[loc]) /
                 sqrt((double) npts*sy2[loc] - (double) sy[loc]*sy[loc]) ;

    // test statistic goes here
    tstatistic[loc] = correl[loc]*sqrt(npts)/(1. - correl[loc]*correl[loc]);
  }
  }
  return;
}
////////////////////
