#include "ssmi.h"
#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  ssmi_bufr_line x;
  global_12th<float> conc;
  global_12th<unsigned char> land;
  mvector<int> histogram[7];
  int i, k, count = 0;
  latpt ll;
  ijpt loc;

  fin = fopen(argv[1], "r");
  conc.binin(fin);
  fclose(fin);
  fin = fopen(argv[2], "r");
  land.binin(fin);
  fclose(fin);
  for (i = 0; i < 7; i++) {
    histogram[i].resize(700);
    histogram[i] = 0;
  }

  fin = fopen(argv[3], "r");
  while (!feof(fin)) {
    fread(&x, sizeof(ssmi_bufr_line), 1, fin);
    if (!feof(fin)) {
      count++;
      for (i = 0; i < NSCANS; i++) {
        ll.lat = x.full[i].latitude;
        ll.lon = x.full[i].longitude;
        loc = conc.locate(ll);
          histogram[0][(int) (x.full[i].t19v+0.5)] += 1;
          histogram[1][(int) (x.full[i].t19h+0.5)] += 1;
          histogram[2][(int) (x.full[i].t22v+0.5)] += 1;
          histogram[3][(int) (x.full[i].t37v+0.5)] += 1;
          histogram[4][(int) (x.full[i].t37h+0.5)] += 1;
          histogram[5][(int) (x.full[i].t85v+0.5)] += 1;
          histogram[6][(int) (x.full[i].t85h+0.5)] += 1;
        printf("%3d %3d  %7.2f %7.2f  %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f\n",(int) (conc[loc]*100+0.5), land[loc], ll.lat, ll.lon,
          x.full[i].t19v,
          x.full[i].t19h,
          x.full[i].t22v,
          x.full[i].t37v,
          x.full[i].t37h,
          x.full[i].t85v,
          x.full[i].t85h
        );
               
        for (k = 0; k < 3; k++) {
          ll.lat = x.full[i].hires[k].latitude;
          ll.lon = x.full[i].hires[k].longitude;
          loc = conc.locate(ll);
          histogram[0][(int) (x.full[i].t19v+0.5)] += 1;
          histogram[1][(int) (x.full[i].t19h+0.5)] += 1;
          histogram[2][(int) (x.full[i].t22v+0.5)] += 1;
          histogram[3][(int) (x.full[i].t37v+0.5)] += 1;
          histogram[4][(int) (x.full[i].t37h+0.5)] += 1;
          histogram[5][(int) (x.full[i].hires[k].t85v+0.5)] += 1;
          histogram[6][(int) (x.full[i].hires[k].t85h+0.5)] += 1;
          printf("%3d %3d  %7.2f %7.2f  %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f\n",(int) (conc[loc]*100+0.5), land[loc], ll.lat, ll.lon,
            x.full[i].t19v,
            x.full[i].t19h,
            x.full[i].t22v,
            x.full[i].t37v,
            x.full[i].t37h,
            x.full[i].hires[k].t85v,
            x.full[i].hires[k].t85h
          );
        }
      }
    }
  }

  for (i = 0; i < 700; i++) {
    printf("%3d  %8d %8d %8d %8d %8d %8d %8d\n",i,
     histogram[0][i],
     histogram[1][i],
     histogram[2][i],
     histogram[3][i],
     histogram[4][i],
     histogram[5][i],
     histogram[6][i]);
  }
  return 0;
}
