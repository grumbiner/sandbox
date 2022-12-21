#include <stdio.h>
#include <math.h>

#define IST  2999
#define IDAY  579
#define STN_ID  1996
// METAR site KMQY = 1996

int main(int argc, char *argv[]) {
  FILE *fgfs[5];
  float t2m[IST], td[IST], rh[IST], wd[IST], th[IST];
  int i,j,k;

  fgfs[0] = fopen("gfs00_t2m_f24h","r");
  fgfs[1] = fopen("gfs00_td_f24h","r");
  fgfs[2] = fopen("gfs00_th_f24h","r");
  fgfs[3] = fopen("gfs00_rh_f24h","r");
  fgfs[4] = fopen("gfs00_wd_f24h","r");

  for (k = 0; k < IDAY; k++) {
      fread(&t2m, sizeof(float), IST, fgfs[0]);
      fread(&td, sizeof(float), IST, fgfs[1]);
      fread(&th, sizeof(float), IST, fgfs[2]);
      fread(&rh, sizeof(float), IST, fgfs[3]);
      fread(&wd, sizeof(float), IST, fgfs[4]);

      printf("%3d %7.2f %7.2f %6.2f %6.2f %5.2f\n",k,
        t2m[STN_ID] - 273.15,
        td[STN_ID]  - 273.15, 
        th[STN_ID], rh[STN_ID], wd[STN_ID] );
  }

  return 0;
}
