#include <stdio.h>
#include "grid3.h"
#include "ncepgrids.h"

int main(int argc, char *argv[]) {

  cfsgrid3<float> s3(19), t3(19), tempor(19);
  cfsgrid<float> layer;

  int i, j, k, nx, ny;
  char fname[800];
  FILE *fin, *fout;
  float avg;

  s3.set(0.0);
  t3.set(0.0);

  sprintf(fname, "tsout.9708%02d", 1);
  fin = fopen(fname, "r");
  t3.binin(fin);
  s3.binin(fin);
  fclose(fin);

  for (i = 2; i <= 31; i++) {
     sprintf(fname, "tsout.9708%02d", i);
     fin = fopen(fname, "r");
     printf("Opening %s\n",fname);

     tempor.binin(fin);
     for (j = 0; j < 19; j++) {
        tempor.get_layer(j, layer);
        avg = layer.average();
        printf("layer average %2d %f\n",j,avg);
        // If we have an unreasonable layer average, replace with
        // the average to date.  Note that we're assuming that the
        // first file is reasonable.
        if (avg < -3.) {
          printf("resetting layer %2d\n",j);
          t3.get_layer(j, layer);
          avg = layer.average();
          printf("new layer average = %f\n", avg);
          layer /= (float)(i-1);
          avg = layer.average();
          printf("new layer average = %f\n", avg);
          tempor.put_layer(j, layer);
        }
     }
     t3 += tempor;

     tempor.binin(fin);
     for (j = 0; j < 19; j++) {
        tempor.get_layer(j, layer);
        avg = layer.average();
        printf("layer average %2d %f\n",j,avg);
        if (avg < -3.) {
          printf("resetting salinity layer %2d\n",j);
          s3.get_layer(j, layer);
          avg = layer.average();
          printf("new layer average = %f\n", avg);
          layer /= (float)(i-1);
          avg = layer.average();
          printf("new layer average = %f\n", avg);
          tempor.put_layer(j, layer);
        }
     }
     s3 += tempor;

     fclose(fin);
  }

    t3 /= (float) 31.0;
    s3 /= (float) 31.0;

  fout = fopen("august.avg", "w");
  t3.binout(fout);
  s3.binout(fout);
  fclose(fout);

  fout = fopen("august.avg.asc", "w");
  t3.printout(fout);
  s3.printout(fout);
  fclose(fout);

  return 0;
}
