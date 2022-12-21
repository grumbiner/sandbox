#include "points.h"
#include "grid_math.h"
#include "ncepgrids.h"

// do harmonic analysis at doodson-like-specified periods
// ignore non-orthogonality for now.
// 13 July 2012

#define NPER 1

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
int main(int argc, char *argv[]) {
// For local reading and accumulating
  GRIDTYPE<float> mean1, tmp;
  GRIDTYPE<double> stmp, stmp2, t1, t2;
  GRIDTYPE<float> ampl, phase;
// For harmonics
  GRIDTYPE<double> sum_sin, sum_cos;
  double fsin, fcos;
  mvector<double> omega(NPER), period(NPER);

// utility:
  FILE *fin;
  int i, j, k;
  double time;

// Initialize:
    sum_sin.set((double) 0);
    sum_cos.set((double) 0);

  fin = fopen("doodson","r");
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
  fclose(fin);

////////////////////////////////////////////////////////////////////////////
// Start scan through observations:
  int index, allcount = 0;
  int counts[12] = { 744, 672, 744, 720, 744, 720, 744, 744, 720, 744, 720, 744 };
  FILE *fout;

  fin = fopen(argv[1],"r");
  mean1.binin(fin);
  fclose(fin);
  mean1 += 273.15;
  conv(mean1, stmp2);
  printf("mean %e %e\n",stmp2.gridmax(), stmp2.gridmin() );

  for (k = 0; k < 12; k++) { 
    fin = fopen(argv[k+2], "r");
    printf("working on month %d  %s\n",k,argv[k+2]); fflush(stdout);
    for (i = 0; i < counts[k]; i++) {
      time = (double)(i+allcount)/(double)24.0;
      if (i%24 == 0) printf("working on day %d %d time = %e\n",k, i/24,time); 
      fflush(stdout);

      tmp.ftnin(fin); // need this because grrr.
      conv(tmp, stmp);
      stmp -= stmp2; // subtract background mean
      t1 = stmp;
      t2 = stmp;

      j = 0;
        fsin = sin(omega[j]*(time) ) * 2./period[j];
        fcos = cos(omega[j]*(time) ) * 2./period[j];

      t1 *= fsin;
      t2 *= fcos;
      sum_sin += t1;
      sum_cos += t2;
      
    }
    fclose(fin);
    allcount += counts[k];
  }
  
  printf("%d hours of obs\n",allcount); fflush(stdout);
// Done doing accumulations
////////////////////////////////////////////////////////////////////////////

// Compute fourier amplitude and phase:
  for (j = 0; j < ampl.ypoints()*ampl.xpoints(); j++) {
     ampl[j] = sqrt(sum_sin[j]*sum_sin[j] +
                    sum_cos[j]*sum_cos[j]    ) / 
                    ((float) allcount / (float) period[0]);
     phase[j] = atan2(sum_sin[j], sum_cos[j] );
  }

  fout = fopen("reference_fields","w");

  j = 0;
  printf("period %7.2f  ampl stats %6.3f %6.3f %6.3f\n",period[j], ampl.gridmax()/1, ampl.average()/1, ampl.rms()/1 );
  fflush(stdout);
   ampl.binout(fout); fflush(fout);
  phase.binout(fout); fflush(fout);

  fclose(fout);

  return 0;

}
