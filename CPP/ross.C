#include <stdio.h>
#include "mvector.h"
#include "time_series.h"

// Ross sea current meter record processing
// Oct  4  1999
// Robert Grumbine

extern "C" void cdtread_(int *hhmm, int *dd, int *mo, int *yy, 
                        float *sp, float *dir, float *u, float *v, 
                        float *temp, float *press, int *seq, int *ierr);

void specprint(mvector<float> &re, mvector<float> &im, FILE *fout) ;
void smooth(mvector<float> &re, mvector<float> &im, int nf) ;

#define MAXOBS 8486

class doodson {
  public:
    int n1, n2, n3, n4, n5, n6;
    float frequency, period;
    doodson();
    ~doodson();
    void set(float );
    doodson(int, int, int, int, int, int);
    void show(FILE *);
  private:
// The following are the estimated frequencies of tidal interest in 
//   cycles per hour.  Note that this gives Doodson mvector different
//   from that used in the previous work, which had been referenced
//   to the lunar day, rather than the solar.
    double  F1 ;
    double  F2 ;
    double  F3 ;
    double  F4 ;
    double  F5 ;
    double  F6 ;
};
doodson::~doodson() {
  n1 = 0;
  n2 = 0;
  n3 = 0;
  n4 = 0;
  n5 = 0;
  n6 = 0;
  frequency = 0.0;
  period = FLT_MAX;
}
doodson::doodson() {
  n1 = 0;
  n2 = 0;
  n3 = 0;
  n4 = 0;
  n5 = 0;
  n6 = 0;
  F1 =  (1./ 24.);
  F2 =  (1./655.7183);
  F3 =  (1./8765.8125);
  F4 =  (F3 / 8.85);
  F5 =  (F3 / 18.6);
  F6 =  (F3 / 21000.);
}
void doodson::set(float x) {
  // Assuming that we're given cycles per hour:
  double rx;
  rx = x;
  frequency = x;
  if (x != 0) {
    period = 1./frequency;
  }
  else {
    period = FLT_MAX;
  }
  n1 = (int) (0.5 + rx / (F1-F2-F3) ); rx -= n1 * (F1-F2+F3) ;
  n2 = (int) (0.5 + rx / F2); rx -= n2 * F2;
  n3 = (int) (0.5 + rx / F3); rx -= n3 * F3;
  n4 = (int) (0.5 + rx / F4); rx -= n4 * F4;
  n5 = (int) (0.5 + rx / F5); rx -= n5 * F5;
  n6 = (int) (0.5 + rx / F6); rx -= n6 * F6;
}
doodson::doodson(int x1, int x2, int x3, int x4, int x5, int x6) {
  n1 = x1;
  n2 = x2;
  n3 = x3;
  n4 = x4;
  n5 = x5;
  n6 = x6;
  F1 =  (1./ 24.);
  F2 =  (1./655.7183);
  F3 =  (1./8765.8125);
  F4 =  (F3 / 8.85);
  F5 =  (F3 / 18.6);
  F6 =  (F3 / 21000.);
  frequency = n6*F6+n5*F5+n4*F4+n3*F3+n2*F2+n1*(F1 - F2 + F3);
  if (frequency != 0) {
    period = 1./frequency;
  }
  else {
    period = FLT_MAX;
  }
}

void doodson::show(FILE *fout) {
  fprintf(fout, "%3d %3d %3d    %3d %3d %5d ",n1, n2, n3, n4, n5, n6);
}


    

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  int hhmm[MAXOBS], dd[MAXOBS], mo[MAXOBS], yy[MAXOBS], seq[MAXOBS];
  int ierr[MAXOBS];
  float sp[MAXOBS], dir[MAXOBS], u[MAXOBS], v[MAXOBS], temp[MAXOBS];
  float press[MAXOBS];
  time_series<float> uvel(MAXOBS), vvel(MAXOBS);
  time_series<float> tempser(MAXOBS), phas(MAXOBS);
  mvector<float> ure(MAXOBS), uim(MAXOBS);
  mvector<float> vre(MAXOBS), vim(MAXOBS);
  mvector<float> tre(MAXOBS), tim(MAXOBS);
  int i, nf;
  float tempor, c0u, c0v, c0t, c0p;
  doodson n(1, 1, -1, 0, 0, 0), m(1, 1, -2, 0,0,0);

  fout = fopen(argv[1], "w");
  printf("Test, 1, 1, -1 has period %f, frequency %f\n",n.period, n.frequency );
  printf("Test, 1, 1, -2 has period %f, frequency %f\n",m.period, m.frequency );

// Read in data
  i = 0; 
  cdtread_(&hhmm[i], &dd[i], &mo[i], &yy[i], &sp[i], &dir[i], &u[i], &v[i], 
              &temp[i], &press[i], &seq[i], &ierr[i]);
  while (ierr[i] == 0 && i < MAXOBS ) {
    i += 1;
    cdtread_(&hhmm[i], &dd[i], &mo[i], &yy[i], &sp[i], &dir[i], &u[i], &v[i], 
              &temp[i], &press[i], &seq[i], &ierr[i]);
  }


// Transfer to time series
  uvel.set(&u[0]);
  vvel.set(&v[0]);
  tempser.set(&temp[0]);

  tempor = uvel.average();
  uvel -= tempor;
  tempor = vvel.average();
  vvel -= tempor;
  tempor = tempser.average();
  printf("Average temperature %f\n",tempser.average() );
  tempser -= tempor;
  printf("Average temperature %f\n",tempser.average() );
  c0u = uvel.autocovary(0);
  c0v = vvel.autocovary(0);
  c0t = tempser.autocovary(0);
  phas = uvel;
  phas *= vvel;
  tempor = phas.average();
  phas -= tempor;
  c0p = phas.autocovary(0);

//  for (i = 0; i < 8450; i++) {
//    printf("%3d %6.3f %6.3f %6.3f %6.3f\n",i,uvel.autocovary(i)/c0u, 
//                                               vvel.autocovary(i)/c0v, 
//                                               tempser.autocovary(i)/c0t , 
//                                               phas.autocovary(i)/c0p ); 
//  }

    // Cross covariances
//  for (i = -8000; i < 8000; i++) {
//     printf("%4d %6.3f %6.3f %6.3f\n", i, 
//                  uvel.crossvary(vvel, i) / sqrt(c0u*c0v), 
//                  uvel.crossvary(tempser, i) / sqrt(c0u*c0t), 
//                  vvel.crossvary(tempser, i) / sqrt(c0v*c0t)   );
//  }

  uvel.fft(ure, uim);
  vvel.fft(vre, vim);
  tempser.fft(tre, tim);

  nf = (int) (0.5 + ure.xpoints() * 1./655.7);  printf("Cycle per day smoothing %d\n",nf);
  smooth(ure, uim, nf);
  smooth(vre, vim, nf);
  smooth(tre, tim, nf);

  specprint(ure, uim, fout); 
  specprint(vre, vim, fout); 
  specprint(tre, tim, fout); 

  return 0;
}
void specprint(mvector<float> &re, mvector<float> &im, FILE *fout) {
  int i;
  doodson n;
  float f;

  n.set(0.0);
  fprintf(fout, "%9.3f %7.5f %8.3f ",(float) 2*re.xpoints(), 0.0,
               (float) log(re[0]*re[0] + im[0]*im[0] ) );
  n.show(fout);
  fprintf(fout,"\n");

  for (i = 1; i < re.xpoints() / 2 + 1; i++) {
    f = (float) i / (float) re.xpoints();
    n.set(f);
    fprintf(fout, "%9.3f %7.5f %8.3f ",(float) re.xpoints()/(float) i,
                   (float) i / (float) re.xpoints(),
                   (float) log(re[i]*re[i] + im[i]*im[i]) );
    n.show(fout);
    fprintf(fout,"\n");
  }
  return;
}
// Smooth a spectrum by running average of adjacent frequencies, nf times
void smooth(mvector<float> &re, mvector<float> &im, int nf) {
  int i, j;
  float tsumr, tsumi;
  mvector<float> sre(re.xpoints() );
  mvector<float> sim(im.xpoints() );

  for (i = 0; i < nf/2; i++) {
    tsumr = 0.;
    tsumi = 0.;
    for (j = 0; j < nf; j++) {
       tsumr += re[i+j];
       tsumi += im[i+j];
    }
    sre[i] = tsumr/(float) nf;
    sim[i] = tsumi/(float) nf;
  }
  for (i = nf/2; i < re.xpoints() - nf/2; i++) {
    tsumr = 0.;
    tsumi = 0.;
    for (j = -nf/2; j < nf/2; j++) {
       tsumr += re[i+j];
       tsumi += im[i+j];
    }
    sre[i] = tsumr/(float) nf;
    sim[i] = tsumi/(float) nf;
  }

  re = sre;
  im = sim;

  return;
}
     
