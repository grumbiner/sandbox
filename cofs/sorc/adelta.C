#include <stdio.h>
#include <math.h>

#include "ncepgrids.h"
#include "cofs.h"

int main(int argc, char *argv[]) {

  cfsreg<float> oldlat, oldlon;
  cfsreg<float> lat, lon;
  ijpt x;

  FILE *fin, *fin2;

  fin = fopen(argv[1], "r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open the input old lat file\n");
    return 1;
  }
  oldlat.reader(fin);

  fin2 = fopen(argv[2], "r");
  if (fin2 == (FILE *) NULL) {
    printf("Failed to open the input new lat file\n");
    return 2;
  }
  lat.reader(fin2);

  printf("Lat \n");
  for (x.j = 0; x.j < lat.ypoints(); x.j++ ) {
  for (x.i = 0; x.i < lat.xpoints(); x.i++) {
    if (fabs(oldlat[x] - lat[x]) > 0.5 && fabs(oldlat[x] - lat[x]) != 99.0 ) {
      printf("%3d %3d  %6.2f %6.2f %7.3f\n",x.i, x.j, oldlat[x], lat[x], oldlat[x] - lat[x] ) ;
    }
  }
  }

  oldlon.reader(fin);
  lon.reader(fin2);
  printf("Lon \n");
  for (x.j = 0; x.j < lon.ypoints(); x.j++ ) {
  for (x.i = 0; x.i < lon.xpoints(); x.i++) {
    if (fabs(oldlon[x] - lon[x]) > 0.5 && fabs(oldlon[x] - lon[x]) != 99.0 ) {
      printf("%3d %3d  %6.2f %6.2f %7.3f\n",x.i, x.j, oldlon[x], lon[x], oldlon[x] - lon[x] ) ;
    }
  }
  }

  return 0;
}
