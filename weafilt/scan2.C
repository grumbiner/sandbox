#include <stdio.h>
#include "mvector.h"

typedef struct {
  float lat, lon, t19v, t19h, t22v, t37v, t37h, t85v, t85h, icecon;
} obs;

void filtration( mvector<mvector<float> > &allobs, int npts) ;

void scorer(mvector<float> &x, mvector<float> &ice, 
            float xmin, float xmax, float step, int c1, int c2) ;

// This is 'scan2' -- it is run against a file which has
//    a) been trimmed down to only points which pass the posteriori grid check
//        (hence would be filtered out even if the ice algorithm passed them)
//    b) Passed the first, c100, filters for ice and water.
// Further development needed because that only classified ca. 75% of input points
// 7 October 2011
//  -- starting point is just to re-run old filterings, in aim of a
//     sequential (crossword puzzle algorithm) filter.


#define MAXOBS (12*1000*1000)

int main(int argc, char *argv[]) {
  FILE *fin;
  obs x;

  double sum1 = 0, sum2=0, sum3=0, sum4=0, sum5=0, sum6=0, sum7 = 0, sumice=0;
  float max1 = 0, max2 = 0, max3 = 0, max4 = 0, max5 = 0, max6 = 0, max7 = 0;
  float maxice = 0;
  float min1 = 2000, min2 = 2000, min3 = 2000, min4 = 2000, min5 = 2000;
  float min6 = 2000, min7 = 2000, minice = 2000;
  int count = 0, ocount = 0, icount = 0, npts;
  int i, j, k;

  mvector<mvector<float> > allobs(8);


// Setup/initialize
  fin = fopen(argv[1], "r");
  if (fin == (FILE *) NULL ) {
    printf("failed to open %s\n",argv[1]);
    return 1;
  }


  for (int i = 0; i < allobs.xpoints(); i++) {
    allobs[i].resize(MAXOBS);
    allobs[i]  = (float) 0.0;
  }

// Perform scan -- max, min, average, 
//    plus transferring data to the big data array for further work
  while (!feof(fin) && count < MAXOBS) {
    //if ( (count % (1000*1000)) == 0) {
    //  printf("count = %d\n",count); fflush(stdout);
    //}

    fread(&x, sizeof(x), 1, fin);
    // x includes lat, lon, but those are being ignored at this point.
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

// Print out the simple statistical scan results:
  printf("averages %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f  %6.2f\n",
        sum1/count, sum2/count, sum3/count,
        sum4/count, sum5/count, sum6/count, sum7/count, sumice/count);
  printf("maxes    %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f  %6.2f \n", 
        max1, max2, max3, max4, max5, max6, max7, maxice);
  printf("mins     %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f  %6.2f \n", 
        min1, min2, min3, min4, min5, min6, min7, minice);

  printf("Counts %d %d %d\n",count, ocount, icount);

// Now start working on data to find suitable filters:
  npts = count - 1;

// Simple critical temperature search:
  filtration(allobs, npts);

// Binary combinations in delta ratio form (Ti - Tj) / (Ti+Tj)
  mvector<float> trial(npts), ice(npts);
  float gmax, gmin, gstep;
  for (i = 0; i < npts; i++) {
    ice[i] = allobs[7][i];
  }
  for (i = 0; i < 7; i++) {
    //if (i == 2) i++;
    for (j = i+1; j < 7; j++) {
      //if (j == 2) j++;
      for (k = 0; k < npts; k++) {
        trial[k] = (allobs[i][k] - allobs[j][k]) / (allobs[i][k] + allobs[j][k]);
        //trial[k] = (allobs[i][k] - allobs[j][k]) ;
      }
      gmax = trial.maximum();
      gmin = trial.minimum();
      gstep = (gmax - gmin) / 100.;
      scorer(trial, ice, gmin, gmax, gstep, i, j);
    }
  }


  return 0;
}

#define ICE 7
void filtration( mvector<mvector<float> > &allobs, int npts) {
  int i, j;
  int foundice, foundwater, missedice, missedwater;
  float tc;

  for (tc = 75.0; tc < 295.0; tc++) {
    for (i = 0; i < ICE; i++) {
      foundice    = 0;
      foundwater  = 0;
      missedice   = 0;
      missedwater = 0;
      for (j = 0; j < npts; j++) {
         if (allobs[i][j] > tc) { 
           if (allobs[ICE][j] > 0) {
             foundice += 1; 
           }
           else {
             foundwater += 1;
           }
         }
         else {
           if (allobs[ICE][j] > 0) {
             missedice += 1; 
           }
           else {
             missedwater += 1;
           }
         }
      } // end of checking points
      float icepod, icefar, waterpod, waterfar;
      icepod = (float) foundice   / (float) (foundice + missedice); 
      icefar = (float) foundwater / (float) (foundice + foundwater);
      waterpod = (float) foundwater   / (float) (foundwater + missedwater); 
      waterfar = (float) foundice     / (float) (foundice + foundwater);
      printf("tc %5.1f i %1d  %9d %9d %9d %9d ice %5.3f %5.3f  water %5.3f %5.3f  foundio %6.3f %10.3f\n",tc, i, 
         foundice, foundwater, missedice, missedwater,
         icepod, icefar, waterpod, waterfar, 
         (float) foundice   / ( (float) foundwater + 1), 
         (float) foundwater / ( (float) foundice   + 1) ); 
    } // end of trying variables 
  } // end of checking the range of critical temperatures

  return;
}
void scorer(mvector<float> &x, mvector<float> &ice, float xmin, float xmax, float step, int c1, int c2) {
  int i, j, npts = x.xpoints();
  int foundice, foundwater, missedice, missedwater;
  float icepod, icefar, waterpod, waterfar;
  float tc;

  for (tc = xmin; tc < xmax; tc += step) {
      foundice    = 0;
      foundwater  = 0;
      missedice   = 0;
      missedwater = 0;
      for (j = 0; j < npts; j++) {
         if (x[j] > tc) { 
           if (ice[j] > 0) {
             foundice += 1; 
           }
           else {
             foundwater += 1;
           }
         }
         else {
           if (ice[j] > 0) {
             missedice += 1; 
           }
           else {
             missedwater += 1;
           }
         }
      } // end of checking points

      icepod = (float) foundice   / (float) (foundice + missedice); 
      icefar = (float) foundwater / (float) (foundice + foundwater);
      waterpod = (float) foundwater   / (float) (foundwater + missedwater); 
      waterfar = (float) foundice     / (float) (foundice + foundwater);
      printf("tc %7.3f c1 c2 %1d %1d %8d %8d %8d %8d ice %5.3f %5.3f  water %5.3f %5.3f  foundio %7.3f %7.3f\n",tc, c1, c2, 
         foundice, foundwater, missedice, missedwater,
         icepod, icefar, waterpod, waterfar, 
         (float) foundice   / ( (float) foundwater + 1), 
         (float) foundwater / ( (float) foundice   + 1) ); 

  } // end of checking the range of critical values

}
