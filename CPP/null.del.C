// Program to score sea ice forecasts

#include "ncepgrids.h"

// Version to build up knowledge from null forecasters.
// Assumes that there is a climatology based on 78 years information
//   and that we are to assess predictions of the last 12 years.
// The climatology includes mean, variance, conditional mean and variance,
//   and the probability of there being ice at the point.

// These includes are so that we can do random access (lseek) access to
//   the history file.
#include <sys/types.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>

// Robert Grumbine 20 August 2002

void score2(grid2<float> &fcst, grid2<float> &obs, grid2<float> &ref, float mindel, 
            grid2<float> &land, grid2<float> &prob, vector<float> &score) ;
float murphy(float *f, float *obs, float *ref, int n) ;

extern "C" float iagree_(float *r, float *x, int &n);
extern "C" float correl_(float *r, float *x, int &n, float &r2, float &xmean,
                        float &ymean, float &sig2x, float &sig2y);
extern "C" float sumx_(float *r, int &n);
extern "C" float sumx2_(float *r, int &n);

// Arguments:
//  1 = history file
//  2 = derived climatology file
//  3 = land file

int main(int argc, char *argv[]) {
  GRIDTYPE<float> fcst, obs, land;
  GRIDTYPE<float> av[12], var[12], condav[12], condvar[12], prob[12];
  vector<float> scores(10);
  vector<float> scoreav[12];
  vector<int>   score_count[12];
  FILE *climin, *flandin;
  float mindel;
  int i, month, lead;
  int fin;
  ijpt loc;

  fin = open(argv[1], O_RDONLY);
  if (fin == -1 ) {
    printf("Failed to open the history file\n");
    return 1;
  }

  climin = fopen(argv[2], "r");
  if (climin == (FILE *) NULL ) {
    printf("Failed to open the climatology file\n");
    return 2;
  }
  
  flandin = fopen(argv[3],"r");
  if (flandin == (FILE *) NULL) {
    printf("Failed to open the reference file\n");
    return 1;
  }
  land.binin(flandin);
  if ( land.average() > 3.0)  land /= 100.;

  mindel = atof(argv[4]);

// Read in climatology:
  for (i = 0; i < 12; i++) {
    av[i].binin(climin);
    var[i].binin(climin);
    condav[i].binin(climin);
    condvar[i].binin(climin);
    prob[i].binin(climin);

    scoreav[i].resize(scores.xpoints() );
    scoreav[i] = (float) 0.;
    score_count[i].resize(scores.xpoints() );
    score_count[i] = (int) 0;
  }

  // edit the conditionals so that they are 0 for p < 0.5
  for (i = 0; i < 12; i++) {
    for (loc.j = 0; loc.j < obs.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < obs.xpoints(); loc.i++) {
      if (prob[i][loc] < 0.5) {
        condav[i][loc] = 0.0;
        condvar[i][loc] = 0.0;
      }
    }
    }
  }


// Now loop over all months in the last 12 years of the history
  for (month = 0; month < 12*12; month++) {
    lseek(fin, (78*12 + month)*sizeof(float)*land.xpoints()*land.ypoints(), 
            SEEK_SET);
    read(fin, obs.grid, sizeof(float)*obs.xpoints()*obs.ypoints()  ); 
    if (obs.gridmax() > 3.0 ) {
      printf("rescaling, gridmax = %f\n", obs.gridmax() );
      obs /= 100.;
      if (obs.gridmax() > 1.0 ) return 1;
    }

//  For each of the null forecasters, in turn, loop over all leads --
//    1-6 months, 1-10 years.
//  For the simple climatology and conditional climatology, no need to loop
//    over leads (forecast is invariant).

//  Simple climatology as fcst:
      lead = 1;
      score2(av[month%12], obs, av[month%12], mindel, land, prob[month%12], scores);
      printf("%1d %3d %3d  %5.3f %5.3f %5.3f %6.3f  %5.3f  %6.3f %5.3f %5.3f  %5.3f\n",
        1, lead, month, scores[0], scores[1], scores[2], scores[3], 
        scores[4], scores[5], scores[6], scores[7], scores[8]);

//  Conditional climatology as fcst:
      lead = 1;
      score2(condav[month%12], obs, av[month%12], mindel, land, prob[month%12], scores);
      printf("%1d %3d %3d  %5.3f %5.3f %5.3f %6.3f  %5.3f  %6.3f %5.3f %5.3f  %5.3f\n",
        2, lead, month, scores[0], scores[1], scores[2], scores[3], 
        scores[4], scores[5], scores[6], scores[7], scores[8]);

// Persistence
    for (lead = 1; lead <= 6; lead++) {
      lseek(fin, (78*12 + month - lead)*sizeof(float)*
            land.xpoints()*land.ypoints(), SEEK_SET);
      read(fin, fcst.grid, sizeof(float)*fcst.xpoints()*fcst.ypoints()  ); 
      score2(fcst, obs, av[month%12], mindel, land, prob[month%12], scores);
      printf("%1d %3d %3d  %5.3f %5.3f %5.3f %6.3f  %5.3f  %6.3f %5.3f %5.3f  %5.3f\n",
        3, lead, month, scores[0], scores[1], scores[2], scores[3], 
        scores[4], scores[5], scores[6], scores[7], scores[8]);
    }
    for (lead = 12; lead <= 120; lead += 12)  {
      lseek(fin, (78*12 + month - lead)*sizeof(float)*
            land.xpoints()*land.ypoints(), SEEK_SET);
      read(fin, fcst.grid, sizeof(float)*fcst.xpoints()*fcst.ypoints()  ); 
      score2(fcst, obs, av[month%12], mindel, land, prob[month%12], scores);
      printf("%1d %3d %3d  %5.3f %5.3f %5.3f %6.3f  %5.3f  %6.3f %5.3f %5.3f  %5.3f\n",
        3, lead, month, scores[0], scores[1], scores[2], scores[3], 
        scores[4], scores[5], scores[6], scores[7], scores[8]);
    }
 

// Anomaly Persistence

  }



  return 0;
}
