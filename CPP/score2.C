#include <stdio.h>
#include <stdlib.h>

class matchup {
  public:
     int year, month, day;
     int skpt, lead;
     float lat1, lon1, lat2, lon2;
     float obs_dist, obs_dir, fcst_dist, fcst_dir;

     matchup(void);
     int read(FILE *);
};
matchup::matchup(void) {
  year = 98; month = 1; day = 1;
  skpt = 0; lead = 0;
  lat1 = 0.; lon1 = 0.; lat2 = 0.; lon2 = 0.;
  obs_dir = 0.; obs_dist = 0.;
  fcst_dir = 0.; fcst_dist = 0.;
}
int matchup::read(FILE *fin) {
  char id[6];
  id[5] = '\0';
  if ( feof(fin) ) {
    return 0;
  }
  fscanf(fin, "%d %d %d %d %d %f %f to %f %f %f %f %f %f %s\n",
    &year, &month, &day, &skpt, &lead, &lat1, &lon1, &lat2, &lon2, 
    &obs_dist, &obs_dir, &fcst_dist, &fcst_dir, &id);
  #ifdef VERBOSE
  printf(
   "%2d %2d %2d %3d %2d %5.2f %6.2f to %5.2f %6.2f %6.2f %6.2f %6.2f %6.2f\n",
    year, month, day, skpt, lead, lat1, lon1, lat2, lon2, 
    obs_dist, obs_dir, fcst_dist, fcst_dir);
  #endif
  return 1;
}

  
extern "C" void ssanaly_(float *odist, float *odir, float *dist, float *dir, 
                              int &npts, float &ia, float &r2, float &vcc);
extern "C" void fit_(float *odist, float *dist, int &n, float &b0, float &b1, 
                     float &correl);
void detrnd(float *fdist, const float b0, const float b1, const int n);

#define MAXBUOYS 80000
int main(int argc, char *argv[]) {
  matchup list[MAXBUOYS];
  float odir[MAXBUOYS], odist[MAXBUOYS], fdir[MAXBUOYS], fdist[MAXBUOYS];
  FILE *fin;
  int i, j, ret, nbuoy, count;
  int fday, skpt, month, year, maxdays;
  float ia, correl, vcc;
  float b0, b1;

  if (argc < 3) {
    printf("Need 2 arguments: file to score, max forecast lead\n");
    return 1;
  }
  fin = fopen(argv[1], "r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open the input file\n");
  }
  maxdays = atoi(argv[2]);
  ret = 1;
  for (i = 0; i < MAXBUOYS && ret != 0; i++) {
     ret = list[i].read(fin);
  }
  nbuoy = i - 1;

////////// Everything all at once:
  for (i = 0; i < nbuoy; i++) {
    odir[i] = list[i].obs_dir;
    odist[i] = list[i].obs_dist;
    fdir[i] = list[i].fcst_dir;
    fdist[i] = list[i].fcst_dist;
  }
  //printf("calling ssanaly\n");fflush(stdout);
  ssanaly_(odist, odir, fdist, fdir, nbuoy, ia, correl, vcc);
  printf("%5d %2d %6.3f %6.3f %6.3f",nbuoy, fday, ia, correl, vcc);
  //printf("calling fit\n");fflush(stdout);
  fit_(fdist, odist, nbuoy, b0, b1, correl);
  printf("  %6.2f %6.3f", b0, b1);
  //printf("calling detrnd\n");fflush(stdout);
  detrnd(fdist, b0, b1, nbuoy);
  //printf("calling ssanaly\n");fflush(stdout);
  ssanaly_(odist, odir, fdist, fdir, nbuoy, ia, correl, vcc);
  printf("  %6.3f %6.3f %6.3f\n",ia, correl, vcc);


////////// Separate by forecast day:
  printf("Count fday IA r VCC  B0 B1  detrended IA r VCC\n");
  for (fday = 1; fday <= maxdays; fday++) {
    count = 0;
    for (i = 0; i < nbuoy; i++) { 
      if (list[i].lead == fday) {
        odir[count] = list[i].obs_dir;
        odist[count] = list[i].obs_dist;
        fdir[count] = list[i].fcst_dir;
        fdist[count] = list[i].fcst_dist;
        count += 1;
      }
    }
    count -= 1;
    if (count >= 10) {
      ssanaly_(odist, odir, fdist, fdir, count, ia, correl, vcc);
      printf("%5d %2d %6.3f %6.3f %6.3f",count, fday, ia, correl, vcc);
      fit_(fdist, odist, count, b0, b1, correl);
      printf("  %6.2f %6.3f", b0, b1);
      detrnd(fdist, b0, b1, count);
      ssanaly_(odist, odir, fdist, fdir, count, ia, correl, vcc);
      printf("  %6.3f %6.3f %6.3f\n",ia, correl, vcc);
    }
    else {
      printf("%4d %2d Insufficient data\n", count, fday);
    }
  }

//Separate by forecast month:
  printf(" \n");
  printf("Count mon IA r VCC  B0 B1  \n");
  for (month = 1; month <= 12; month++) {
    count = 0;
    for (i = 0; i < nbuoy; i++) { 
      //if (list[i].month == month && list[i].lead == 6 ) {
      if (list[i].month == month ) {
        odir[count] = list[i].obs_dir;
        odist[count] = list[i].obs_dist;
        fdir[count] = list[i].fcst_dir;
        fdist[count] = list[i].fcst_dist;
        count += 1;
      }
    }
    count -= 1;
    if (count >= 10) {
      ssanaly_(odist, odir, fdist, fdir, count, ia, correl, vcc);
      printf("%5d %2d %6.3f %6.3f %6.3f",count, month, ia, correl, vcc);
      fit_(fdist, odist, count, b0, b1, correl);
      printf("  %6.2f %6.3f\n", b0, b1);
    }
    else {
      printf("%5d %2d Insufficient data\n", count, month);
    }
  }

//Separate by skiles point
  printf("\n Skiles point verification\n");
  printf("count fday IA r VCC B0 B1 IA' R' VCC' \n");
  // Note that fday is now skiles point
  for (skpt = 1; skpt <= 207; skpt++) {
    for (fday = 1; fday <= maxdays; fday++) {
      count = 0;
      for (i = 0; i < nbuoy; i++) { 
        if (list[i].skpt == skpt && list[i].lead == fday ) {
          odir[count] = list[i].obs_dir;
          odist[count] = list[i].obs_dist;
          fdir[count] = list[i].fcst_dir;
          fdist[count] = list[i].fcst_dist;
          count += 1;
        }
      }
      count -= 1;
      if (count > 3) {
        printf("pt %3d day %2d  ",skpt, fday);
        ssanaly_(odist, odir, fdist, fdir, count, ia, correl, vcc);
        printf("%5d %2d %6.3f %6.3f %6.3f",count, fday, ia, correl, vcc);
        fit_(fdist, odist, count, b0, b1, correl);
        printf("  %6.2f %6.3f", b0, b1);
        detrnd(fdist, b0, b1, count);
        ssanaly_(odist, odir, fdist, fdir, count, ia, correl, vcc);
        printf("  %6.3f %6.3f %6.3f\n",ia, correl, vcc);
      }
    }
  }

  return 0;
}
void detrnd(float *fdist, const float b0, const float b1, const int count) {
  int i;
  for (i = 0; i < count; i++) {
    fdist[i] = b0 + b1 * fdist[i];
  }
  return;
}
