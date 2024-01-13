#include <math.h>
#include "ncepgrids.h"

// 10 March 2001

#define PERIOD (12*5)

static mvector<mvector<float> > sines(PERIOD/2+1), cosines(PERIOD/2+1);
extern void sledgeft(mvector<float> &test, mvector<float> &sincoef, 
                                           mvector<float> &coscoef) ;
extern void toampl(mvector<float> &sincoef, mvector<float> &coscoef,
            mvector<float> &ampl, mvector<float> &phase);

int main(int argc, char *argv[]) {
  northgrid<float> iceinfo[PERIOD];
  northgrid<float> ampls[PERIOD/2+1], phases[PERIOD/2+1];

  mvector<float> test(PERIOD), sincoef(PERIOD/2+1), coscoef(PERIOD/2+1);
  mvector<float> ampl(PERIOD/2+1), phase(PERIOD/2+1);

  int i, n, period = PERIOD, startyear;
  char fname[900];
  FILE *fin;
  ijpt loc;
  palette<unsigned char> gg(19,65);

//Set up the trig series
  sines.resize(PERIOD/2+1);
  cosines.resize(PERIOD/2+1);
  for (n = 0; n < period/2+1; n++) {
    sines[n].resize(PERIOD);
    cosines[n].resize(PERIOD);
    for (i = 0; i < period; i++) {
      sines[n][i] = sin(2.*M_PI*n *i / period);
      cosines[n][i] = cos(2.*M_PI*n *i / period);
    }
  }  

//Get the data grids:
  startyear = atoi(argv[1]);
  for (i = 0; i < period; i++ ) {
    sprintf(fname, "%s.%02d.bin", argv[1], i+1);
    sprintf(fname, "month.%4d%02d.bin", startyear + i/12, i%12+1);
    fin = fopen(fname, "r");
    if (fin == (FILE *) NULL) {
      printf("failed to open %s\n",fname);
    }
    iceinfo[i].binin(fin);
    fclose(fin);
  }

  for (loc.j = 0; loc.j < iceinfo[0].ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < iceinfo[0].xpoints(); loc.i++) {
    for (i = 0; i < period; i++) {
      test[i] = iceinfo[i][loc];
    }
    sledgeft(test, sincoef, coscoef);
    toampl(sincoef, coscoef, ampl, phase);
    for (i = 0; i < period/2+1; i++) {
      //printf("%d  %f sin cos %f %f\n",i, (float) PERIOD / (float) i, 
      //printf("%d  %f sin cos %f %f A theta %f %f\n",
      //          i, (float) i / (float) PERIOD ,
      //          sincoef[i], coscoef[i], ampl[i], phase[i]);
      ampls[i][loc] = ampl[i];
      phases[i][loc] = phase[i];
    }
  }
  }

  for (i = 0; i < period/2+1; i++) {
    printf("harmonic %d max, min %f %f\n",i, ampls[i].gridmax(), ampls[i].gridmin() );
    ampls[i].scale();
    sprintf(fname, "ampl%d.xpm",i);
    ampls[i].xpm(fname, 7, gg);
  }

  return 0;
}
