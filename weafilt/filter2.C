#include <stdio.h>
#include "mvector.h"

typedef struct {
  float lat, lon, t19v, t19h, t22v, t37v, t37h, t85v, t85h, icecon;
} obs;

#define MAXOBS (52*1000*1000)

int main(int argc, char *argv[]) {
  FILE *fin;
  obs x;

  double sum1 = 0, sum2=0, sum3=0, sum4=0, sum5=0, sum6=0, sum7 = 0, sumice=0;
  float max1 = 0, max2 = 0, max3 = 0, max4 = 0, max5 = 0, max6 = 0, max7 = 0, maxice = 0;
  float min1 = 2000, min2 = 2000, min3 = 2000, min4 = 2000, min5 = 2000, min6 = 2000, min7 = 2000, minice = 2000;
  int count = 0, ocount = 0, icount = 0, npts;
  int i;

  mvector<mvector<float> > allobs(8);


// Setup/initialize
  fin = fopen(argv[1], "r");
  if (fin == (FILE *) NULL ) {
    printf("failed to open %s\n",argv[1]);
    return 1;
  }

  for (i = 0; i < allobs.xpoints(); i++) {
    allobs[i].resize(MAXOBS);
    allobs[i]  = (float) 0.0;
  }

// Perform scan -- max, min, average, 
//    plus transferring data to the big data array for further work
  while (!feof(fin) && count < MAXOBS) {

    fread(&x, sizeof(x), 1, fin);

    sum1 += x.t19v; max1 = max(max1, x.t19v); min1 = min(min1, x.t19v);
    sum2 += x.t19h; max2 = max(max2, x.t19h); min2 = min(min2, x.t19h);
    sum3 += x.t22v; max3 = max(max3, x.t22v); min3 = min(min3, x.t22v);
    sum4 += x.t37v; max4 = max(max4, x.t37v); min4 = min(min4, x.t37v);
    sum5 += x.t37h; max5 = max(max5, x.t37h); min5 = min(min5, x.t37h);
    sum6 += x.t85v; max6 = max(max6, x.t85v); min6 = min(min6, x.t85v);
    sum7 += x.t85h; max7 = max(max7, x.t85h); min7 = min(min7, x.t85h);
    sumice += x.icecon; maxice = max(maxice, x.icecon); minice = min(minice, x.icecon);
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

  }
  npts = count - 1;

// Print out the simple statistical scan results:
  printf("averages %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f  %6.2f\n",
        sum1/count, sum2/count, sum3/count,
        sum4/count, sum5/count, sum6/count, sum7/count, sumice/count);
  printf("maxes    %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f  %6.2f \n", 
        max1, max2, max3, max4, max5, max6, max7, maxice);
  printf("mins     %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f  %6.2f \n", 
        min1, min2, min3, min4, min5, min6, min7, minice);

  printf("Counts %d %d %d\n",count, ocount, icount);

// Now start trying out filters:
  float twater = 233.0;
  //float tice   = 199.0;
  float tice   = 224.0;
  int nwater = 0, nice = 0, nboth = 0;
//
//  for (i = 0; i < npts; i++) {
//    if (allobs[5][i] > twater) {
//      nwater++;
//    }
//    if (allobs[1][i] > tice) {
//      nice++;
//    }
//    if (allobs[5][i] > twater && allobs[1][i] > tice) {
//      //printf("pass both water and ice filter!\n");
//      nboth++;
//    }
//  }
//  printf("%8d  %8d %8d  %8d %8d\n",npts, nwater, nice, nboth, 
//                                   npts - nwater - nice + nboth);


// Water filters from binary deltas:
  nwater = 0; nice = 0; nboth = 0;
  
  float w1crit = 0.2142, w2crit = 0.0, w3crit = 0.189, w4crit = 0.0;
  int wi1 = 0, wj1 = 1, wi2 = 0, wj2 = 4, wi3 = 3, wj3 = 4, wi4 = 5, wj4 = 6;
  int nw1 = 0, nw2 = 0, nw3 = 0, nw4 = 0;
  float g1, g2, g3, g4;

  float i1, i2, i3, i4;
  int ni1 = 0, ni2 = 0, ni3 = 0, ni4 = 0;
  // bests float i1crit = 0.023, i2crit = -0.0099, i3crit = 0.0289, i4crit = 0.0;
  // c400 or best:
  //float i1crit = -0.003, i2crit = -0.052, i3crit = 0.0289, i4crit = 0.0;
  // c300 or best:
  //float i1crit = -0.015, i2crit = -0.075, i3crit = 0.009, i4crit = 0.0;
  // c200 
  //float i1crit = -0.03, i2crit = -0.11, i3crit = -0.005, i4crit = -0.04;
  // c100 
  float i1crit = -0.055, i2crit = -0.16, i3crit = -0.023, i4crit = -0.079;
  int ii1 = 0, ii2 = 1, ii3 = 3, ii4 = 4;
  int ij1 = 5, ij2 = 5, ij3 = 5, ij4 = 5;


  for (i = 0; i < npts; i++) {
    g1 = (allobs[wi1][i] - allobs[wj1][i]) / (allobs[wi1][i] + allobs[wj1][i]);
    g2 = (allobs[wi2][i] - allobs[wj2][i]) / (allobs[wi2][i] + allobs[wj2][i]);
    g3 = (allobs[wi3][i] - allobs[wj3][i]) / (allobs[wi3][i] + allobs[wj3][i]);
    g4 = (allobs[wi4][i] - allobs[wj4][i]) / (allobs[wi4][i] + allobs[wj4][i]);

    i1 = (allobs[ii1][i] - allobs[ij1][i]) / (allobs[ii1][i] + allobs[ij1][i]);
    i2 = (allobs[ii2][i] - allobs[ij2][i]) / (allobs[ii2][i] + allobs[ij2][i]);
    i3 = (allobs[ii3][i] - allobs[ij3][i]) / (allobs[ii3][i] + allobs[ij3][i]);
    i4 = (allobs[ii4][i] - allobs[ij4][i]) / (allobs[ii4][i] + allobs[ij4][i]);

    if (i1 > i1crit || i2 > i2crit || i3 > i3crit || i4 > i4crit) {
      nice++;
      if (i1 > i1crit) ni1++;
      if (i2 > i2crit) ni2++;
      if (i3 > i3crit) ni3++;
      if (i4 > i4crit) ni4++;
    }
    if (g1 > w1crit || g3 > w3crit) {
      nwater++;
      if (g1 > w1crit) nw1++;
      if (g3 > w3crit) nw3++;
    }
    if (  (i1 > i1crit || i2 > i2crit || i3 > i3crit || i4 > i4crit)
        && (g1 > w1crit || g3 > w3crit) ) {
      nboth++;
    }
  }
  printf("%8d  %8d %8d  %8d %8d\n",npts, nwater, nice, nboth, 
                                   npts - nwater - nice + nboth);

  printf("w1 %8d\n",nw1);
  printf("w3 %8d\n",nw3);
  printf("i1 %8d\n",ni1);
  printf("i2 %8d\n",ni2);
  printf("i3 %8d\n",ni3);
  printf("i4 %8d\n",ni4);

  return 0;
}
