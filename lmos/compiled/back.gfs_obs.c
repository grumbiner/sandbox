#include <stdio.h>
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
  int count = 0;
  float t2mdelta, tddelta;

  fgfs[0] = fopen("gfs00_t2m_f24h","r");
  fgfs[1] = fopen("gfs00_td_f24h","r");
  fgfs[2] = fopen("gfs00_th_f24h","r");
  fgfs[3] = fopen("gfs00_rh_f24h","r");
  fgfs[4] = fopen("gfs00_wd_f24h","r");
  fobs[0] = fopen("obs_t2m","r");
  fobs[1] = fopen("obs_td","r");

  for (k = 0; k < IDAY_GFS; k++) {
      fread(&t2m, sizeof(float), IST_GFS, fgfs[0]);
      fread(&td, sizeof(float), IST_GFS, fgfs[1]);
      fread(&th, sizeof(float), IST_GFS, fgfs[2]);
      fread(&rh, sizeof(float), IST_GFS, fgfs[3]);
      fread(&wd, sizeof(float), IST_GFS, fgfs[4]);

      fread(&obs_t2m, sizeof(float), IST_OBS, fobs[0]);
      fread(&obs_td,  sizeof(float), IST_OBS, fobs[1]);

      if (obs_t2m[STN_ID] < 999. && obs_td[STN_ID] < 999.0) {
        count += 1;
        t2mdelta = (t2m[STN_ID] - 273.15)-(obs_t2m[STN_ID]-32.0)/1.8 ;
        tddelta  = (td [STN_ID] - 273.15)-(obs_td [STN_ID]-32.0)/1.8 ;
        sum2m += t2mdelta;
        sum2msq += t2mdelta*t2mdelta;
        sumd    += tddelta;
        sumdsq  += tddelta*tddelta;

      printf("%3d %7.2f %7.2f %6.2f %6.2f %5.2f  %7.2f %7.2f  %f %f\n",k,
        t2m[STN_ID] - 273.15,
        td[STN_ID]  - 273.15, 
        th[STN_ID], rh[STN_ID], wd[STN_ID],
        (obs_t2m[STN_ID]-32.0)/1.8, (obs_td[STN_ID]-32.0)/1.8,
// predict the deviation from straight model output:
        t2mdelta, tddelta);
      }

// obs are 8*day while gfs is only 1*day
      for (i = 1; i < 8; i++) {
        fread(&obs_t2m, sizeof(float), IST_OBS, fobs[0]);
        fread(&obs_td, sizeof(float), IST_OBS, fobs[1]);
      }
  }

  printf("means %f %f  rms %f %f  var %f %f\n",sum2m/count, sumd/count,
          sqrt(sum2msq/count), sqrt(sumdsq/count), 
          sqrt((sum2msq*count - sum2m*sum2m)/(count-1)/count), sqrt((sumdsq*count - sumd*sumd)/(count-1)/count) );


  return 0;
}
