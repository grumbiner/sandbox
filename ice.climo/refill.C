#include "ncepgrids.h"
#include "time_series.h"
#include <errno.h>

#define MAXPTS  57817


int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  mvector<float> avg(MAXPTS), count(MAXPTS), sd(MAXPTS), norm(MAXPTS);
  mvector<int> index(MAXPTS);
  mvector<ijpt> loc(MAXPTS);
  mvector<latpt> ll(MAXPTS);

  mvector<float> correl(MAXPTS);
  global_ice<float> corin;

  int i, j, ti, tj, points;
  int countin, indexin;
  float ain, sdin;
  float lat, lon, d1, f1, f2, f3;
  ijpt tloc;
  latpt tll;


// Scan the summary file for point locations
  fin = fopen("allice.summary","r");
  if (fin == (FILE*) NULL) {
    printf("failed to open allice.summary\n");
    return 1;
  }
  for (i = 0; i < MAXPTS; i++) {
    fscanf(fin, "%d %d %d %f %f %d %f %f %f %f %f %f\n",&indexin, &ti, 
                &tj, &lat, &lon, &countin, &ain, &d1, &sdin, &f1, &f2, &f3);
    index[i] = indexin;
    tloc.i = ti; tloc.j = tj;
    loc[i] = tloc;
    tll.lat = lat; tll.lon = lon;
    ll[i]   = tll;
    count[i] = (float) countin;
    avg[i] = ain;
    sd[i]  = sqrt(sdin);
  }
  fclose(fin);

  printf("read in allice, last countmax = %f\n",count.maximum() );


// Now read in the correlation vectors, filling in blanks for the ones with fewer than 1000 points:
  int nfile;
  char fname[900];

  fin  = (FILE*) NULL;
  fout = (FILE*) NULL;

  for (i = 0; i < MAXPTS; i++) {
    //printf("in distribution loop\n"); fflush(stdout);
    if ( (i%9000) == 0) {
      printf("file management, i = %d\n",i); fflush(stdout);

      nfile = i/9000;
      if (fin != (FILE*) NULL) fclose(fin);
      sprintf(fname, "correlout.%1d",nfile);
      fin = fopen(fname,"r");
      if (fin == (FILE *) NULL) {
        printf("failed to open input %s\n",fname);
        return 1;
      }
      if (fout != (FILE*) NULL) fclose(fout);
      sprintf(fname, "reformed.%1d",nfile);
      fout = fopen(fname,"w");
      if (fout == (FILE *) NULL) {
        printf("failed to open output %s\n",fname);
        return 1;
      }

    }
    if (count[i] >= 1000) {
      correl.binin(fin);
      correl.binout(fout);
    }
    else {
      correl = 0.0;
      correl.binout(fout);
    }

  }

  fclose(fin);
  fclose(fout);

  
  return 0;
}
