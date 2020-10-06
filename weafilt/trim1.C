#include <stdio.h>
#include "mvector.h"

typedef struct {
  float lat, lon, t19v, t19h, t22v, t37v, t37h, t85v, t85h, icecon;
} obs;

#define MAXOBS (52*1000*1000)

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  obs x;
  mvector<mvector<float> > allobs(8);

  int count = 0, ocount = 0, icount = 0, npts;
  int i;
  int nice = 0, nwater = 0, nboth = 0, nwrite = 0;

  float w1crit = 0.2142;
  float i1crit = -0.055;
  int wi1 = 0, wj1 = 1;
  int ii1 = 0, ij1 = 5;
  float g1, i1;

// Setup/initialize
  fin = fopen(argv[1], "r");
  if (fin == (FILE *) NULL ) {
    printf("failed to open %s\n",argv[1]);
    return 1;
  }

  fout = fopen(argv[2], "w");
  if (fout == (FILE *) NULL ) {
    printf("failed to open %s\n",argv[2]);
    return 2;
  }


  for (i = 0; i < allobs.xpoints(); i++) {
    allobs[i].resize(MAXOBS);
    allobs[i]  = (float) 0.0;
  }

// Perform scan and write back out those which the filters don't catch/classify
//   uniquely
//    plus transferring data to the big data array for further work
  while (!feof(fin) && count < MAXOBS) {

    fread(&x, sizeof(x), 1, fin);

    count += 1;
    if (x.icecon > 0. && x.icecon < 1.28) {
       icount += 1;
    }
    else if (x.icecon == 0.) {
       ocount += 1;
    }
    else {
       printf("erroneous concentration! %f\n",x.icecon);
    }

    allobs[0][count] = x.t19v; 
    allobs[1][count] = x.t19h; 
    allobs[2][count] = x.t22v; 
    allobs[3][count] = x.t37v; 
    allobs[4][count] = x.t37h; 
    allobs[5][count] = x.t85v; 
    allobs[6][count] = x.t85h; 
    allobs[7][count] = x.icecon; 

    g1 = (allobs[wi1][count] - allobs[wj1][count]) / (allobs[wi1][count] + allobs[wj1][count]);
    i1 = (allobs[ii1][count] - allobs[ij1][count]) / (allobs[ii1][count] + allobs[ij1][count]);
    if (i1 > i1crit ) {
      nice++;
    }
    if (g1 > w1crit ) {
      nwater++;
    }

    if (  (i1 > i1crit ) && (g1 > w1crit )  ) {
      nboth++;
      fwrite(&x, sizeof(x), 1, fout);
      nwrite++;
    }
    if (i1 <= i1crit && g1 <= w1crit) {
      fwrite(&x, sizeof(x), 1, fout);
      nwrite++;
    }


  }

  printf("Counts %d %d %d\n",count, ocount, icount);
  printf("Nwritten = %d\n",nwrite);


  return 0;
}
