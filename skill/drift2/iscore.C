#include <cstdio>
#include <cstdlib>
#include <cmath>

//#include "buoy.h"

// Note that there is a limit near here on number of points
//   which can be processed before a segfault occurs.  It
//   makes no sense that this is the case.
#define MAXBUOYS (300*1000 + 32*1024 )

extern "C" void ssanaly_(float *odist, float *odir, float *dist, float *dir, 
                              int &npts, float &ia, float &r2, float &vcc);
extern "C" void fit_(float *odist, float *dist, int &n, float &b0, float &b1, 
                     float &correl);

#include "subs.C"

int main(int argc, char *argv[]) {
  float odir[MAXBUOYS], odist[MAXBUOYS], fdir[MAXBUOYS], fdist[MAXBUOYS];
  float smodir[MAXBUOYS], smodist[MAXBUOYS], smfdir[MAXBUOYS], smfdist[MAXBUOYS];
  int   ptno[MAXBUOYS];
  FILE *fin;
  int i, ret, nbuoy, count, skpt;
  float lat, lon, diro, disto, dirf, distf;
  float ia, correl, vcc;
  float b0, b1;
  float meandist, meandir, rmsdist, rmsdir, errad, erradrms;


  fin = fopen(argv[1], "r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open the input file\n");
    return 1;
  }
  ret = 1;
  for (i = 0; (i < MAXBUOYS) && (ret != 0); i++) {
     fscanf(fin, "%d %f %f %f %f %f %f\n", &skpt, &lat, &lon, &diro, &disto, &dirf, &distf);
     ptno[i]  = skpt;
     odir[i]  = diro;
     fdir[i]  = dirf;
     odist[i] = disto;
     fdist[i] = distf;
     //printf("i = %d\n",i); fflush(stdout);
     if (feof(fin)) break;
  }
  nbuoy = i - 1;
  //printf("past the read loop, nbuoy = %d\n",nbuoy); fflush(stdout);


// All forecasts at once:
  ssanaly_(odist, odir, fdist, fdir, nbuoy, ia, correl, vcc);
  printf("%5d  %6.3f %6.3f %6.3f",nbuoy, ia, correl, vcc);
  fit_(fdist, odist, nbuoy, b0, b1, correl);
  printf("  %6.2f %6.3f", b0, b1);
  rms(odist, odir, fdist, fdir, nbuoy, meandist, meandir, rmsdist, 
                       rmsdir, errad, erradrms);
  printf("  %6.2f %6.1f  %5.1f %5.1f %5.1f %5.1f\n",meandist, meandir, rmsdist, 
                       rmsdir, errad, erradrms);


////////Separate by skiles point
  printf("\n Skiles point verification\n");
  for (skpt = 1; skpt <= 207; skpt++) {
    count = 0;
    for (i = 0; i < nbuoy; i++) { 
      if (ptno[i] == skpt ) {
        smodir[count]  = odir[i];
        smodist[count] = odist[i];
        smfdir[count]  = fdir[i];
        smfdist[count] = fdist[i];
        count += 1;
      }
    }
    count -= 1;
    if (count > 3) {
      printf("pt %3d ",skpt);
      ssanaly_(smodist, smodir, smfdist, smfdir, count, ia, correl, vcc);
      printf("%5d %6.3f %6.3f %6.3f",count, ia, correl, vcc);
      fit_(smfdist, smodist, count, b0, b1, correl);
      printf("  %6.2f %6.3f", b0, b1);
      rms(smodist, smodir, smfdist, smfdir, count, meandist, meandir, rmsdist, rmsdir, errad, erradrms);
      printf("  %6.2f %6.1f  %5.1f %5.1f %5.1f %5.1f\n",meandist, meandir, rmsdist, rmsdir, errad, erradrms);
    }
  }

  return 0;
}
