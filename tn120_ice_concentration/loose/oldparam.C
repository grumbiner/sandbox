#include <stdio.h>

#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin1, *fin2, *fout;
  nsidcnorth<short int> t37v, t37h;
  nsidcnorth<float> param;
  ijpt loc;
  int count = 0;

  //printf("size of short int = %d\n",sizeof(short int) );
  fin1 = fopen(argv[1], "r");
  fin2 = fopen(argv[2], "r");
  fout = fopen(argv[3], "w");
  if (fin1 == (FILE*)NULL) {
    printf("Failed to open %s\n",argv[1]);
    return 1;
  }
  if (fin2 == (FILE*)NULL) {
    printf("Failed to open %s\n",argv[2]);
    return 1;
  }
  if (fout == (FILE*)NULL) {
    printf("Failed to open %s\n",argv[3]);
    return 1;
  }

  t37h.binin(fin1);
  t37v.binin(fin2);
  fclose(fin1);
  fclose(fin2);

  printf("t37v stats %d %d  %d %d\n", 
       t37v.gridmax(-1.), t37v.gridmin(-1.), t37v.average(-1.), t37v.rms(-1.) );
  printf("t37h stats %d %d  %d %d\n", 
       t37h.gridmax(-1.), t37h.gridmin(-1.), t37h.average(-1.), t37h.rms(-1.) );

  for (loc.j = 0; loc.j < param.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < param.xpoints(); loc.i++) {
    if (t37h[loc] > t37v[loc] || t37h[loc] < 1000. || t37v[loc] < 1500. 
                              || t37h[loc] > 2950. || t37v[loc] > 2950. ) {
      // printf("error %d %d  %d %d\n",loc.i, loc.j, t37v[loc], t37h[loc]);
      param[loc] = -1.;
      count += 1;
    }
    else {
      param[loc] = sqrt( t37v[loc]*t37v[loc] - t37h[loc]*t37h[loc] ) / 10.;
    }
  }
  } 
  printf("error count %d\n",count);

  printf("max, min, avg, rms %f %f %f %f\n",
        param.gridmax(-1.), param.gridmin(-1.), param.average(-1.), 
        param.rms(-1.) );

  param.binout(fout);
  fclose(fout);

  return 0;

}
