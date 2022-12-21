#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define IST_GFS  2999
#define IST_OBS  3001
#define IDAY_GFS  579
#define IDAY_OBS  669

#define STN_ID  1995
// METAR site KMQY = 1996

int main(int argc, char *argv[]) {
  FILE *fgfs[5], *fobs[2];
  float t2m[IST_GFS], td[IST_GFS], rh[IST_GFS], wd[IST_GFS], th[IST_GFS];
  float obs_t2m[IST_OBS], obs_td[IST_OBS]; 
  int i,j,k;
  double sum2m=0.0, sumd=0.0, sum2msq=0.0, sumdsq=0.0;
  int count = 0, station;
  float t2mdelta, tddelta;

  fobs[0] = fopen("obs_t2m","r");
  fobs[1] = fopen("obs_td","r");

  station = atoi(argv[1]);

  for (k = 0; k < IDAY_OBS; k++) {

    for (j = 0; j < 8; j++) {
      fread(&obs_t2m, sizeof(float), IST_OBS, fobs[0]);
      fread(&obs_td,  sizeof(float), IST_OBS, fobs[1]);

      if (obs_t2m[station] < 999. && obs_td[station] < 999.0) {
        count += 1;
        sum2m += obs_t2m[station];
        sumd  += obs_td[station];
        sum2msq += obs_t2m[station]*obs_t2m[station];
        sumdsq  += obs_td[station]*obs_td[station];
        printf("%5d\t%6.1f\t%6.1f\n",(k*24 + j*3),
            obs_t2m[station], obs_td[station]          );
        }
      }

  }

//  printf("means %.2f %.2f  rms %.2f %.2f  sqrtvar %.2f %.2f\n",sum2m/count, sumd/count,
//          sqrt(sum2msq/count), sqrt(sumdsq/count), 
//          sqrt((sum2msq*count - sum2m*sum2m)/(count-1)/count), sqrt((sumdsq*count - sumd*sumd)/(count-1)/count) );


  return 0;
}
