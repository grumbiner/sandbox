#include "ncepgrids.h"

#define NMRF 53
#define NSSMI 21
#define TOTAL 74

#define MRF_LAND 31
#define MRF_ICE 32

#define ALLTYPE 0
#define LONLY 1
#define IONLY 2
#define OONLY 3
#define ALLOCE 4 

void tovec(metricgrid<float> &dat, float *x, float nonval, int *count) ;
extern "C" void correl(float *a1, float *a2, const int k,
              float *r2, float *xbar, float *ybar, float *sig2x, float *sig2y);

int main(int argc, char *argv[]) {
  northgrid<float> nh[NMRF + NSSMI];
  southgrid<float> sh[NMRF + NSSMI];
  FILE *finmrf, *finssmi, *nout, *sout;
  float x[465*385], y[465*385];
  float nonval = 99990002605540088217.0;

  float r2, xbar, ybar, sig2x, sig2y;

  grid2<float> ntable(TOTAL,TOTAL);
  grid2<float> stable(TOTAL,TOTAL);
  int i, j;
  int count, nx_north, nx_south, ny_north, ny_south;
  ijpt loc;

  finmrf = fopen(argv[1], "r");
  finssmi = fopen(argv[2], "r");
  nout = fopen(argv[3], "w");
  if (finmrf == (FILE*) NULL || finssmi == (FILE*) NULL || 
      nout == (FILE*) NULL) {
    printf("Failed to open a north file\n");
    return 1;
  }

  nx_north = nh[0].xpoints();
  ny_north = nh[0].ypoints();
  for (i = 0; i < NMRF ; i++) {
    nh[i].binin(finmrf);
    printf("n %2d %f\n",i, nh[i].average(nonval) );
  }
  fclose(finmrf);
  for (i = NMRF; i < NMRF + NSSMI; i++) {
    nh[i].binin(finssmi);
    printf("n %2d %f\n",i, nh[i].average(nonval) );
  } 
   
  fclose(finssmi);

  //printf("land q %2d %f\n",MRF_LAND, nh[MRF_LAND].average(nonval) );
  //printf("sice q %2d %f\n",MRF_ICE, nh[MRF_ICE].average(nonval) );


// Southern hemisphere:
  nx_south = sh[0].xpoints();
  ny_south = sh[0].ypoints();
  finmrf = fopen(argv[4], "r");
  finssmi = fopen(argv[5], "r");
  sout = fopen(argv[6], "r");
  for (i = 0; i < NMRF; i++) {
    sh[i].binin(finmrf);
    printf("s %2d %f\n",i, sh[i].average(nonval) );
  }
  fclose(finmrf);
  for (i = NMRF; i < NMRF+NSSMI; i++) {
    sh[i].binin(finssmi);
    printf("s %2d %f\n",i, sh[i].average(nonval) );
  }
  fclose(finssmi);
 
// given all data, construct all pairs of vectors and correlate:
// North
  for (i = 0 ; i < TOTAL-1; i++) {
    tovec(nh[i], x, nonval, &count);    
    for (j = i+1; j < TOTAL; j++) {
      tovec(nh[j], y, nonval, &count);
      correl(x, y, nx_north*ny_north, &r2, &xbar, &ybar, &sig2x, &sig2y) ;
      fprintf(nout,"%2d %2d %f\n",i, j, r2);
      if (fabs(r2*r2) > 0.1) {
        printf("n %2d %2d %f\n",i, j, r2*r2);
      }
    }
  }
// south
  for (i = 0 ; i < TOTAL-1; i++) {
    tovec(sh[i], x, nonval, &count);    
    printf("%2d %d\n",i,count);
    for (j = i+1; j < TOTAL; j++) {
      tovec(sh[j], y, nonval, &count);
      printf("%2d %d\n",j,count);
      correl(x, y, count, &r2, &xbar, &ybar, &sig2x, &sig2y) ;
      fprintf(sout,"%2d %2d %f\n",i, j, r2);
      if (fabs(r2*r2) > 0.1) {
        printf("s %2d %2d %f\n",i, j, r2*r2);
      }
    }
  }


  return 0;
}

void tovec(metricgrid<float> &dat, float *x, float nonval, int *count) {
  int i, lcount = 0;

  for (i = 0; i < dat.xpoints()*dat.ypoints(); i++) {
    if (dat[i] != nonval) {
      x[lcount] = dat[i];
      lcount += 1;
    }
  }
  *count = lcount;
}
