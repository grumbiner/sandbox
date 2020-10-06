#include "points.h"
#include "ncepgrids.h"

// Do harmonic analysis to find annual cycle
// Also compute linear fit, and examine residual mean (should be < 0.01K)

#define PERIOD 365.259636
#define NPER    74


void compute_harmonics(mvector<global_quarter<double> > &sum_sin, mvector<global_quarter<double> > &sum_cos, mvector<global_quarter<float> > &ampl, mvector<global_quarter<float> > &phase, int npts) ;

template <class T>
void compute_trends(grid2<T> &sx, grid2<T> &sx2, grid2<T> &sxy, grid2<T> &sy, grid2<T> &sy2, grid2<float> &slope, grid2<float> &intercept, grid2<float> &correl, grid2<float> &tstatistic, int npts) ;


int main(int argc, char *argv[]) {
// For reading and accumulating
  global_quarter<short int> tmp;
  global_quarter<long long int> stmp, sumy, sumy2;
// For trend
  global_quarter<long long int> sumx, sumx2, sumxy;  
// For harmonics
  mvector<global_quarter<double> > sum_sin(NPER), sum_cos(NPER);
  double fsin[NPER], fcos[NPER];

// utility:
  FILE *fin;
  int i, j, npts;
  ijpt loc;


// Start working
  npts = argc - 1;
  printf("npts = %d\n",npts); fflush(stdout);

// Initialize:
  sumy.set((long long int) 0);
  sumy2.set((long long int) 0);
  sumx.set((long long int) 0);
  sumxy.set((long long int) 0);
  sumx2.set((long long int) 0);
  for (i = 0; i < NPER; i++) {
    sum_sin[i].set((double) 0);
    sum_cos[i].set((double) 0);
  }


// Start scan through observations:
  for (i = 1; i < argc; i++) {
    fin = fopen(argv[i], "r");
    tmp.binin(fin);
    fclose(fin);
    printf("read in file %d\n",i); fflush(stdout);
    conv(tmp, stmp);

    // for harmonics
    for (j = 1; j <= NPER; j++) {
      fsin[j-1] = sin(2.*M_PI*j*(i-1) / PERIOD) * 2./PERIOD;
      fcos[j-1] = cos(2.*M_PI*j*(i-1) / PERIOD) * 2./PERIOD;
    }

    for (int index = 0; index < sumy.xpoints()*sumy.ypoints(); index++) {
      // for trends:
      sumy[index]  += stmp[index];
      sumy2[index] += stmp[index]*stmp[index];
      sumx[index]  += (i-1);
      sumx2[index] += (i-1)*(i-1);
      sumxy[index] += stmp[index]*(i-1);

      // for harmonics
      for (j = 0; j < NPER; j++) {
        sum_sin[j][index] += stmp[index]*fsin[j];
        sum_cos[j][index] += stmp[index]*fcos[j];
      }
    }

  }
// Done doing accumulations

// Find things of interest and write them out:
  FILE *fout;

  global_quarter<float> slope, intercept, correl, tstatistic;
  compute_trends(sumx, sumx2, sumxy, sumy, sumy2, slope, intercept, correl, tstatistic, npts);

  fout = fopen("annual_harmonics_out", "w");

  slope.binout(fout);
  intercept.binout(fout);
  correl.binout(fout);
  tstatistic.binout(fout);
  fflush(fout);

// Defer this declaration for memory considerations
  mvector<global_quarter<float> > ampl(NPER), phase(NPER);
  compute_harmonics(sum_sin, sum_cos, ampl, phase, npts);
  global_quarter<long long int> tvar;
  global_quarter<float> var;

  for (int index = 0; index < var.xpoints()*var.ypoints(); index++) {
    tvar[index] = (sumy2[index] - sumy[index]*sumy[index] / npts );
    var[index] = (float) tvar[index] / (float) (npts*100*100);
  }
  var.binout(fout);
  for (j = 0; j < NPER; j++) {
    ampl[j].binout(fout); fflush(fout);
    phase[j].binout(fout); fflush(fout);
  }
  
  fclose(fout);
  return 0;
}


////////////////////
void compute_harmonics(mvector<global_quarter<double> > &sum_sin, mvector<global_quarter<double> > &sum_cos, mvector<global_quarter<float> > &ampl, mvector<global_quarter<float> > &phase, int npts) {
  ijpt loc;
  int i;
// Compute fourier amplitude and phase:
  for (loc.j = 0; loc.j < ampl[0].ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ampl[0].xpoints(); loc.i++) {
    for (i = 0; i < NPER; i++) {
       ampl[i][loc] = sqrt(sum_sin[i][loc]*sum_sin[i][loc] +
                           sum_cos[i][loc]*sum_cos[i][loc]    ) / ((float) npts / (float) PERIOD);
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
    slope[loc] = (float) ((double) (npts*sxy[loc] - sx[loc]*sy[loc]) /
                          (double) (npts*sx2[loc] - sx[loc]*sx[loc])  ) ;
    intercept[loc] = (sy[loc]/npts - slope[loc]*sx[loc]/npts);
    correl[loc] =    (npts*sxy[loc] - sx[loc]*sy[loc]) /
                 sqrt(npts*sx2[loc] - sx[loc]*sx[loc]) /
                 sqrt(npts*sy2[loc] - sy[loc]*sy[loc]) ;

    // test statistic goes here
    tstatistic[loc] = correl[loc]*sqrt(npts)/(1. - correl[loc]*correl[loc]);
  }
  }
  return;
}
