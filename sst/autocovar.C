#include "ncepgrids.h"

#define NLAG 35

int main(int argc, char *argv[] ) {
  FILE *fin;
  global_quarter<float> tin[NLAG], tmp, mean;
  global_quarter<double> sumsq[NLAG], dtmp, dtmp2;
  int i, lag, lag0, lagr;

// find autocorrelations at lags 0-34
// first step being to sum product deviations from mean lag0*lagN in sumsq[N]
//   N = 0..34

// assumed to have zero mean on input, else would have to subtract that from all fields
// prep loop i = 1, NLAG-1, then working accumulation loop NLAG..argc-1
  printf("nlag = %d argc = %d\n",NLAG, argc);
  for (i = 1; i < NLAG; i++) {
    printf("fname %s\n",argv[i]); fflush(stdout);
    fin = fopen(argv[i],"r");
    tin[i-1].binin(fin);
    fclose(fin); 
    sumsq[i-1].set(0.0);
  }
  sumsq[NLAG-1].set(0.0);
  mean.set((float) 0.0);

  for (i = NLAG; i < argc; i++) {
    lag0 = (i-1)     % NLAG;
    printf("fname %s lag0 %d \n",argv[i], lag0); fflush(stdout);

    fin = fopen(argv[i],"r");
    tin[lag0].binin(fin);
    fclose(fin); 
    mean += tin[lag0];

    // do this modulo business so that the inputs are in a rotating stack of grids
    for (lag = 0; lag < NLAG; lag++) {
      lagr = (i-1-lag) % NLAG;
      tmp  = tin[lag0];
      tmp *= tin[lagr];
      conv(tmp, dtmp);
      sumsq[lag] += dtmp;
    }

  }
  for (lag = 0; lag < NLAG; lag++) {
    sumsq[lag] /= (double) (argc - NLAG);
  }
  mean /= (float) (argc - NLAG);
  printf("mean stats: %f %f %f %f\n",mean.gridmax(), mean.gridmin(), mean.average(), mean.rms() );

// Now do some minor outputting:
  FILE *fout;
  fout = fopen("autocorrels","w");
  dtmp = sumsq[0];

  for (lag = 0; lag < NLAG; lag++) {
    dtmp2 = sumsq[lag];
    printf("var lag %2d max etc. %f %f %f %f\n",lag, dtmp2.gridmax(), dtmp2.gridmin(), dtmp2.average(), dtmp2.rms() ); 

    for (i = 0; i < dtmp.xpoints()*dtmp.ypoints(); i++) {
      if (dtmp[i] != 0) {
        dtmp2[i] /= dtmp[i];
      }
    }

    printf("autocorrel %2d max etc. %f %f %f %f\n",lag, dtmp2.gridmax(), dtmp2.gridmin(), dtmp2.average(), dtmp2.rms() ); 
    
    conv(dtmp2, tmp);
    tmp.binout(fout);
  }
  fclose(fout);

  return 0;
}
