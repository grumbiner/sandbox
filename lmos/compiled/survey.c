#include <stdio.h>
#include <math.h>

#define IST_OBS  3001
#define IDAY_OBS  669

#define STN_ID  1995
// METAR site KMQY = 1996 #ftn index

int main(int argc, char *argv[]) {
  FILE *fgfs[5], *fobs[2];
  float obs_t2m[IST_OBS], obs_td[IST_OBS]; 
  double sum[IST_OBS], sumsq[IST_OBS];
  int i,j,k;
  int count[IST_OBS] ;

  fobs[0] = fopen("obs_t2m","r");
  fobs[1] = fopen("obs_td","r");

  for (k = 0; k < IST_OBS; k++) {
    count[k] = 0;
    sum[k]   = 0.0;
    sumsq[k] = 0.0;
  }

  for (k = 0; k < IDAY_OBS; k++) {

      for (j = 0; j < 8; j++) {
        fread(&obs_t2m, sizeof(float), IST_OBS, fobs[0]);
        fread(&obs_td,  sizeof(float), IST_OBS, fobs[1]);

        for (i = 0; i < IST_OBS; i++) {
          if (obs_t2m[i] < 999. && obs_td[i] < 999.0) {
            count[i] += 1;
            sum[i] += (t2m[i] - 273.15)-(obs_t2m[i]-32.0)/1.8 ;
            sumsq[i] += ((t2m[i] - 273.15)-(obs_t2m[i]-32.0)/1.8)*((t2m[i] - 273.15)-(obs_t2m[i]-32.0)/1.8) ;
          }
        }
      }
  }

  for (k = 0; k < IST_OBS; k++) {
    if (count[k] > 1) {
      printf("%4d  %3d  %6.2f %5.2f %5.2f\n",k, count[k], sum[k]/count[k], sqrt(sumsq[k]/count[k]), 
             sqrt((sumsq[k]*count[k] - sum[k]*sum[k])/(count[k]-1)/count[k]));
    }
  }


  return 0;
}
