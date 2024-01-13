#include <math.h>
#include "ncepgrids.h"

//11 March 2001

#define PERIOD (12*1)
extern void sledgeft(mvector<float> &test, mvector<float> &sincoef, 
                                           mvector<float> &coscoef) ;


static mvector<mvector<float> > sines(PERIOD/2), cosines(PERIOD/2);

int main(int argc, char *argv[]) {
  northgrid<float> iceinfo[PERIOD];
  northgrid<float> sinwt[PERIOD/2], coswt[PERIOD/2];

  mvector<float> test(PERIOD), sincoef(PERIOD/2), coscoef(PERIOD/2);
  mvector<float> ampl(PERIOD/2), phase(PERIOD/2);

  int i, n, period = PERIOD;
  char fname[900];
  FILE *fin, *fout;
  ijpt loc;

//Set up the trig series
  sines.resize(PERIOD/2);
  cosines.resize(PERIOD/2);
  for (n = 0; n < period/2; n++) {
    sines[n].resize(PERIOD);
    cosines[n].resize(PERIOD);
    for (i = 0; i < period; i++) {
      sines[n][i] = sin(2.*M_PI*n *i / period);
      cosines[n][i] = cos(2.*M_PI*n *i / period);
    }
  }  

//Get the data grids:
  for (i = 0; i < period; i++ ) {
    printf("Trying to read in month %d\n",i+1);
    sprintf(fname, "avgmonth.%02d.bin", i+1);
    fin = fopen(fname, "r");
    if (fin == (FILE *) NULL) {
      printf("failed to open %s\n",fname);
      return 1;
    }
    iceinfo[i].binin(fin);
    fclose(fin);
  }

  for (loc.j = 0; loc.j < iceinfo[0].ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < iceinfo[0].xpoints(); loc.i++) {
    printf("Working on point %d %d\n",loc.i, loc.j); fflush(stdout);
    for (i = 0; i < period; i++) {
      test[i] = iceinfo[i][loc];
    }
    sledgeft(test, sincoef, coscoef);
    //toampl(sincoef, coscoef, ampl, phase);
    for (i = 0; i < period/2; i++) {
      sinwt[i][loc] = sincoef[i];
      coswt[i][loc] = coscoef[i];
    }
  }
  }
  
  printf("About to try to construct the daily fields\n");
  fflush(stdout);
// Now that we have trig coefficients, compute ice fields for each day
//  of the year.
  {
    northgrid<float> daygrid, tmp1, tmp2;
    int t, j;
    float c, s;
    palette<unsigned char> gg(19,65);

    for (t = 0; t < 365; t++) {
      daygrid.set(0.0);
      for (j = 0; j < PERIOD/2; j++) {
        c = cos( 2.*M_PI/365.*j * (t-30.) );  //The minus 30 is needed because
        s = sin( 2.*M_PI/365.*j * (t-30.) );  // the atlas is end of month figures
        tmp1 = sinwt[j];
        tmp1 *= s;
        tmp2 = coswt[j];
        tmp2 *= c;
        tmp1 += tmp2;
        daygrid += tmp1;
       }
       //Now loop through and ensure that concentrations are in 0-1 range
       printf("day %d average is %f\n",t, daygrid.average() );
       for (loc.j = 0; loc.j < daygrid.ypoints(); loc.j++) {
       for (loc.i = 0; loc.i < daygrid.xpoints(); loc.i++) {
         if (daygrid[loc] > 100. && daygrid[loc] < 150.) daygrid[loc] = 100.;
         if (daygrid[loc] < 0.)   daygrid[loc] = 0.0;
       }
       }
       sprintf(fname, "day.%03d.xpm",t+1);
       daygrid.xpm(fname, 7, gg);

       daygrid /= 100.0;
       sprintf(fname, "day.%03d.bin",t+1);
       fout = fopen(fname, "w");
       daygrid.binout(fout);
       fclose(fout);
       
     }  // End of day loop
   } //end of fourier recomputation section


  return 0;
}
