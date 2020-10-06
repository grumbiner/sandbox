#include "ncepgrids.h"
#include "ssmi.h"

typedef struct {
  float lat[NSCANS*4], lon[NSCANS*4];
  float t19v[NSCANS*4];
  float t19h[NSCANS*4];
  float t22v[NSCANS*4];
  float t37v[NSCANS*4];
  float t37h[NSCANS*4];
  float t85v[NSCANS*4];
  float t85h[NSCANS*4];
} obs;

typedef struct {
  float lat, lon, t19v, t19h, t22v, t37v, t37h, t85v, t85h;
} short_obs;

typedef struct {
  float lat, lon, t19v, t19h, t22v, t37v, t37h, t85v, t85h, icecon;
} obs2;

int main(int argc, char *argv[]) {
  FILE *finland, *finice, *finssmi, *foutmatch;
  global_12th<float> icecon;
  global_12th<unsigned char> land;
  ijpt loc;
  latpt ll;
  int i, count = 0;
  short_obs x;
  obs2 y;

  finland = fopen(argv[1], "r");
  land.binin(finland);
  fclose(finland);

  finice = fopen(argv[2], "r");
  icecon.binin(finice);
  fclose(finice);

  finssmi = fopen(argv[3], "r");
  foutmatch = fopen(argv[4], "w");
  printf("ready to start working on data\n"); fflush(stdout);

  while (!feof(finssmi) ) {
    fread(&x, sizeof(short_obs), 1, finssmi);
      ll.lat = x.lat;
      ll.lon = x.lon;
      loc = icecon.locate(ll);
      if (land[loc] == 0) {
        y.lat = x.lat;
        y.lon = x.lon;
        y.t19v = x.t19v;
        y.t19h = x.t19h;
        y.t22v = x.t22v;
        y.t37v = x.t37v;
        y.t37h = x.t37h;
        y.t85v = x.t85v;
        y.t85h = x.t85h;
        y.icecon = icecon[loc];
        // only print out those which pass a gross range validity check
        if (y.t19v > 75.0 && y.t19h > 75.0 && y.t22v > 75.0 && 
            y.t37v > 75.0 && y.t37h > 75.0 && y.t85v > 75.0 && y.t85h > 75.0) {
        if (y.t19v < 295.0 && y.t19h < 295.0 && y.t22v < 295.0 && 
            y.t37v < 295.0 && y.t37h < 295.0 && y.t85v < 295.0 && y.t85h < 295.0) {
          fwrite(&y, sizeof(y), 1, foutmatch);
        }
        }
        count++;
      }
    
  }
  fclose(finssmi);
  fclose(foutmatch);

  printf("Found %d points to work with\n",count);

  return 0;
}
