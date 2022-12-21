#include "points.h"
#include "ncepgrids.h"

// do harmonic analysis at doodson-like-specified periods
// ignore non-orthogonality for now.
// 13 July 2012

#define NPER 30

#define TROPICAL    365.24219
#define SIDEREAL    365.256363
#define ANOMALISTIC 365.259635

#define lunar_sidereal 27.32166
#define lunar_perigee (8.85*SIDEREAL)
#define lunar_node    (18.6*SIDEREAL)

template<class T>
class t382 : public grid2<T> {
  public:
    t382();
};
template<class T>
t382<T>::t382() {
  this->nx = 1152;
  this->ny =  576;
  this->grid = new T[this->nx*this->ny];
}


////////////////////////////////////////////////////////////////////////
template <class T>
void compute_trends(grid2<T> &sx, grid2<T> &sx2, grid2<T> &sxy, grid2<T> &sy, grid2<T> &sy2, grid2<float> &slope, grid2<float> &intercept, grid2<float> &correl, grid2<float> &tstatistic, int npts) ;

template <class T>
void assemble2(grid2<T> &expectation, grid2<T> &slope, grid2<T> &intercept, mvector<grid2<T> > &ampl, mvector<grid2<T> > &phase, float &time, int &nharm, mvector<double> &omega) ;

template <class T>
void assemble1(grid2<T> &expectation, grid2<T> &slope, grid2<T> &intercept, float time) ;
////////////////////////////////////////////////////////////////////////

int main(int argc, char *argv[]) {
// For local reading and accumulating
  GRIDTYPE<float> mean1, tmp;
  GRIDTYPE<double> stmp, stmp2, sumy, sumy2;
// For local trend
  GRIDTYPE<double> sumx, sumx2, sumxy;  
// For harmonics
  mvector<GRIDTYPE<double> > sum_sin(NPER), sum_cos(NPER);
  double fsin[NPER], fcos[NPER];
  mvector<double> omega(NPER), period(NPER);
// From first pass
  GRIDTYPE<float> slope, intercept;

// utility:
  FILE *fin;
  int i, j, npts, nharm = NPER;
  ijpt loc;
  double time;
  GRIDTYPE<float> expect;

///////////////////////////////////////////////////////////
// Start working
  npts = argc - 2;
  printf("npts = %d\n",npts); fflush(stdout);

// Initialize:
  sumy.set((double) 0);
  sumy2.set((double) 0);
  sumx.set((double) 0);
  sumxy.set((double) 0);
  sumx2.set((double) 0);
  for (i = 0; i < NPER; i++) {
    sum_sin[i].set((double) 0);
    sum_cos[i].set((double) 0);
  }

  fin = fopen("doodson","r");
  if (fin == (FILE*) NULL) {
    printf("Failed to open doodson file!\n");
    return 1;
  }
  int d1, d2, d3, d4, d5, d6;
  for (j = 0; j < NPER; j++) {
    fscanf(fin, "%d %d %d %d %d %d\n",&d1, &d2, &d3, &d4, &d5, &d6);
    printf("%d %d %d %d %d %d  ",d1, d2, d3, d4, d5, d6);
    omega[j] = 
        1           *d1        + 1/(lunar_sidereal)*d2  +
        1/(ANOMALISTIC)*d3     + 1/(TROPICAL)*d4        +
        1/(lunar_node)*d5      + 1/(lunar_perigee)*d6 ;
     period[j] = 1./omega[j];
     omega[j] *= 2.*M_PI;
     printf(" %e %e\n",period[j],omega[j]);
  }
  fflush(stdout);

////////////////////////////////////////////////////////////////////////////
  int index, k, allcount = 0;
  int counts[12] = { 744, 672, 744, 720, 744, 720, 744, 744, 720, 744, 720, 744 };
  FILE *fout;

  fin = fopen(argv[1],"r");
  mean1.binin(fin);
  fclose(fin);
  mean1 += 273.15;
  conv(mean1, stmp2);
  printf("read in the mean, max = %f\n",mean1.gridmax() ); fflush(stdout);

// Start scan through observations:
  for (k = 0; k < 12; k++) {
    fin = fopen(argv[k+2], "r");
    printf("working on month %d  %s\n",k,argv[k+2]); fflush(stdout);
    for (i = 0; i < counts[k]; i++) {
      time = (double)(i+allcount)/(double)24.0;
      if (i%24 == 0) printf("working on day %d %d time = %e\n",k, i/24,time);
      fflush(stdout);

      tmp.ftnin(fin);
      conv(tmp, stmp);
      stmp -= stmp2;

      // for harmonics
      for (j = 0; j < NPER; j++) {
        fsin[j] = sin(omega[j]*(time) ) * 2./period[j];
        fcos[j] = cos(omega[j]*(time) ) * 2./period[j];
      }

      for (index = 0; index < sumy.xpoints()*sumy.ypoints(); index++) {
        // for trends:
        sumy[index]  += stmp[index];
        sumy2[index] += stmp[index]*stmp[index];
        sumx[index]  += (double) (time);
        sumx2[index] += (double) (time*time);
        sumxy[index] += stmp[index]*(double) (time);
  
        // for harmonics
        for (j = 0; j < NPER; j++) {
          sum_sin[j][index] += stmp[index]*fsin[j];
          sum_cos[j][index] += stmp[index]*fcos[j];
          }
      }

    }
    fclose(fin);
    allcount += counts[k];
  }
// Done doing accumulations
////////////////////////////////////////////////////////////////////////////

// Find things of interest and write them out:
  GRIDTYPE<float> lslope, lintercept, correl, tstatistic;
  compute_trends(sumx, sumx2, sumxy, sumy, sumy2, lslope, lintercept, correl, tstatistic, npts);

  printf("lslope max min avg rms %f %f %f %f\n",lslope.gridmax(), lslope.gridmin(), lslope.average()*allcount, lslope.rms()*allcount );
  printf("lintercept max min avg rms %f %f %f %f\n",lintercept.gridmax(), lintercept.gridmin(), lintercept.average(), lintercept.rms() );
  fflush(stdout);

  slope     += lslope;
  intercept += lintercept;


  fout = fopen("reference_fields","w");
  intercept.binout(fout);
  slope.binout(fout);

// Defer this declaration for memory considerations
  mvector<GRIDTYPE<float> > ampl(NPER), phase(NPER);

  for (loc.j = 0; loc.j < ampl[0].ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ampl[0].xpoints(); loc.i++) {
    for (i = 0; i < NPER; i++) {
       ampl[i][loc] = sqrt(sum_sin[i][loc]*sum_sin[i][loc] +
                           sum_cos[i][loc]*sum_cos[i][loc]    ) 
                       / ((float) allcount / (float) period[i]);
       phase[i][loc] = atan2(sum_sin[i][loc], sum_cos[i][loc] );
    }
  }
  }


  for (j = 0; j < NPER; j++) {
    printf("period %8.3f  ampl stats %6.3f %5.3f %5.3f\n",period[j], ampl[j].gridmax(), ampl[j].average(), ampl[j].rms() );
    fflush(stdout);
    ampl[j].binout(fout);  fflush(fout);
    phase[j].binout(fout); fflush(fout);
  }
  
  fclose(fout);
//////////////////////////////////////////////////////////////////////


  return 0;
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
template <class T>
void assemble1(grid2<T> &expectation, grid2<T> &slope, grid2<T> &intercept, float time) {
// time is days since 1 September 1981, with that day being 0.
  expectation = slope;
  expectation *= time;
  expectation += intercept;
  return;
}

template <class T>
void assemble2(grid2<T> &expectation, grid2<T> &slope, grid2<T> &intercept, mvector<grid2<T> > &ampl, mvector<grid2<T> > &phase, float &time, int &nharm, mvector<double> &omega) {
// time is days since 1 September 1981, with that day being 0.

  expectation = slope;
  expectation *= time;
  expectation += intercept;

  for (int index = 0; index < expectation.ypoints()*expectation.xpoints(); index++) {
    for (int j = 0; j < nharm; j++) {
      expectation[index] += ampl[j][index]*cos(omega[j]*time + phase[j][index] );
    }
  }
  return;
}
