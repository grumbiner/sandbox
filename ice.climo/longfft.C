#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "ncepgrids.h"

//16 March 2001

#define PERIOD (12*90)
#define NX 385
#define NY 465
#define DELTA (36*1024*1024 / PERIOD / 4/ 2 / NX )

extern void sledgeft(mvector<float> &test, mvector<float> &sincoef, 
                                           mvector<float> &coscoef) ; 
extern void toampl(mvector<float> &sincoef, mvector<float> &coscoef,
            mvector<float> &ampl, mvector<float> &phase);

static mvector<mvector<float> > sines(PERIOD/2), cosines(PERIOD/2);

int main(int argc, char *argv[]) {
  grid2<float> iceinfo(NX, NY);
  //The business with DELTA is so that we can be simple minded and just
  //  work with entire grids.  The penalty is that we have to read in the
  //  original data NY/DELTA + 1 times.
  //grid2<float> proc(NX, DELTA)[PERIOD];
  grid2<float> *proc[PERIOD];
  grid2<float> *ampls[PERIOD/2];
  grid2<float> *phases[PERIOD/2];
  grid2<float> tproc(NX, DELTA);

  mvector<float> test(PERIOD), sincoef(PERIOD/2), coscoef(PERIOD/2);
  mvector<float> ampl(PERIOD/2), phase(PERIOD/2);

  int i, n, period = PERIOD, startyear;
  int pass, nread, ymax;
  char fname[900];
  FILE *fin;
  ijpt loc;
  palette<unsigned char> gg(19,65);

// Allocate the grids:
  printf("Number of lines at a time: %d\n", DELTA);
  for (i = 0; i < PERIOD; i++) {
     proc[i] = new grid2<float>(NX, DELTA);
  }
  for (i = 0; i < PERIOD/2; i++) {
     ampls[i] = new grid2<float>(NX, DELTA);
     phases[i] = new grid2<float>(NX, DELTA);
  }


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

  //startyear = atoi(argv[1]);
  startyear = 1901;
//Get the data grids: -- Loop so as to avoid overrunning memory.  Read in
//  data, pull out a manageable strip, and process that.  This would
//  parallelize very well.
  for (pass = 0; pass < iceinfo.ypoints() / DELTA + 1; pass++) {
    ymax = min(iceinfo.ypoints()-1, (pass+1)*DELTA-1) ;
    for (i = 0; i < period; i++ ) {
      sprintf(fname, "month.%4d%02d.bin", startyear + i/12, i%12+1);
      fin = fopen(fname, "r");
      if (fin == (FILE *) NULL) {
        printf("failed to open %s\n",fname);
	return 1;
      }
      printf("Trying to read %s\n",fname); fflush(stdout);
      nread = iceinfo.binin(fin);
      if (nread != iceinfo.xpoints() * iceinfo.ypoints()) {
        printf("Failed to read in sufficient data, got %d of %d\n",
                nread, iceinfo.xpoints() * iceinfo.ypoints() );
        return 2;
      }
      fclose(fin);

      proc[i]->subset(iceinfo, 0, pass*DELTA, iceinfo.xpoints()-1, ymax );
    }

    // Loop over all grid points in subset and compute fourier coefficients
    for (loc.j = 0; loc.j < proc[0]->ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < proc[0]->xpoints(); loc.i++) {
      for (i = 0; i < period; i++) {
        test[i] = proc[i]->operator[](loc);
      }
      sledgeft(test, sincoef, coscoef);
      toampl(sincoef, coscoef, ampl, phase);
      for (i = 0; i < period/2; i++) {
        ampls[i]->operator[](loc) = ampl[i];
        phases[i]->operator[](loc) = phase[i];
      }
    }
    }
    //Now that we have a new grid set of amplitudes and phases, put them in to
    //  the appropriate segments of files:
    if (pass == 0) {
      iceinfo.set(0.0);
      for (i = 0; i < PERIOD/2; i++) {
        sprintf(fname, "ampl.%03d.bin", i);
        fin = fopen(fname,"w");
        iceinfo.binout(fin);
        fclose(fin);
    
        sprintf(fname, "phase.%03d.bin", i);
        fin = fopen(fname,"w");
        iceinfo.binout(fin);
        fclose(fin);
      }
    }

    for (i = 0; i < period/2 ; i++) {
      sprintf(fname, "ampl.%03d.bin", i);
      fin = fopen(fname,"r+");
      if (fin == (FILE *) NULL) {
        printf("Failed to open %s\n",fname);
        return 3;
      }
      nread = iceinfo.binin(fin);
      if (nread != iceinfo.xpoints() * iceinfo.ypoints() ) {
        printf("Failed to read in data from %s\n",fname);
        return 3;
      }
      iceinfo.enter(*ampls[i], 0, pass*DELTA, iceinfo.xpoints() -1 , 
                             ymax );
      rewind(fin);
      iceinfo.binout(fin);
      fclose(fin);
    
      sprintf(fname, "phase.%03d.bin", i);
      fin = fopen(fname,"r+");
      if (fin == (FILE *) NULL) {
        printf("Failed to open %s\n",fname);
        return 3;
      }
      iceinfo.binin(fin);
      if (nread != iceinfo.xpoints() * iceinfo.ypoints() ) {
        printf("Failed to read in data from %s\n",fname);
        return 3;
      }
      iceinfo.enter(*phases[i], 0, pass*DELTA, iceinfo.xpoints() -1 , 
                             ymax );
      rewind(fin);
      iceinfo.binout(fin);
      fclose(fin);
    }
  } // End of the data passes

  for (i = 0; i < period/2; i++) {
    printf("harmonic %d max, min %f %f\n",i, ampls[i]->gridmax(), ampls[i]->gridmin() );
    ampls[i]->scale();
    sprintf(fname, "ampl%d.xpm",i);
    ampls[i]->xpm(fname, 7, gg);
  }

  return 0;
}
