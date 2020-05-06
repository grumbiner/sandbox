#include "ncepgrids.h"
#include "time_series.h"
#include <errno.h>


#define MAXPTS  57817
#define MAXDAYS 10957 

float dot(mvector<float> &x, mvector<float> &y) ;

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  global_ice<float> corout;
  mvector<float> avg(MAXPTS), count(MAXPTS), sd(MAXPTS), norm(MAXPTS);
  mvector<int> index(MAXPTS);
  mvector<ijpt> loc(MAXPTS);
  mvector<latpt> ll(MAXPTS);
  mvector<float> correl(MAXPTS);
  int i, j, ti, tj;
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
    fscanf(fin, "%d %d %d %f %f %d %f %f %f %f %f %f\n",&indexin, &ti, &tj, &lat, &lon, &countin, &ain, &d1, &sdin, &f1, &f2, &f3);
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

  // want to get ith vector from massive array correlout
  i = atoi(argv[1]); // seek to the ith spot
  printf("i = %d\n", i); fflush(stdout);

  fin = fopen("tmp","r"); 
  fseek(fin, i*4*MAXPTS, SEEK_SET);
  correl.binin(fin);
  fclose(fin);

  printf("max correl %f %f %f %f\n",correl.maximum(), correl.minimum(), correl.average(), correl.rms() );

  corout.set((float) 0.0);
  for (i = 0; i < MAXPTS; i++) {
    corout[loc[i] ] = correl[i];
  }

  fout = fopen("mapout","w");
  corout.binout(fout);
  fclose(fout);
  

  return 0;
}

float dot(mvector<float> &x, mvector<float> &y) {
  double sum = 0;
  
  for (int i = 0; i < x.xpoints(); i++) {
    sum += x[i]*y[i];
  }
  return (float) sum;
}
