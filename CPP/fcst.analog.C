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

// Robert Grumbine 20 August 2002

#define REFYEARS 78
// Variant to search out and display analogs
// Revised: Variant to evaluate the skill of analog forecasting

void score2(grid2<float> &fcst, grid2<float> &obs, 
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
  GRIDTYPE<float> fcst, obs, land, tmp;
  GRIDTYPE<float> av[12], var[12], condav[12], condvar[12], prob[12];
  vector<float> scores(10);
  vector<float> scoreav[12], allscores[REFYEARS];
  vector<int>   score_count[12], bests(10);
  FILE *climin, *flandin;
  int i, j, month, lead;
  int fin;
  ijpt loc;
  palette<unsigned char> gg(19,65);
  char fname[900];

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

  for (i = 0; i < REFYEARS; i++) {
    allscores[i].resize(scores.xpoints() );    
  }

// Now loop over all months in the last 12 years of the history
  for (month = 0; month < 12*12; month++) {

//  Get the current month's information:
    lseek(fin, (REFYEARS*12 + month)*sizeof(float)*land.xpoints()*
          land.ypoints(), SEEK_SET);
    read(fin, obs.grid, sizeof(float)*obs.xpoints()*obs.ypoints()  ); 
    if (obs.gridmax() > 3.0 ) {
      printf("rescaling, gridmax = %f\n", obs.gridmax() );
      obs /= 100.;
      if (obs.gridmax() > 1.0 ) return 1;
    }
//  Now loop over all possible analogous months and save their scores:
    for (i = 0; i < REFYEARS; i++) {
      lseek(fin, (i*12 + month%12)*sizeof(float)*land.xpoints()*
            land.ypoints(), SEEK_SET);
      read(fin, fcst.grid, sizeof(float)*obs.xpoints()*obs.ypoints() );
      if (fcst.gridmax() > 3.0 ) {
        fcst /= 100.;
        if (fcst.gridmax() > 1.0 ) return 1;
      }
      score2(fcst, obs, land, prob[month%12], scores);
      allscores[i] = scores;
    } 
//  Loop back through to find bests per score type
    bests = 0;
    for (i = 1; i < REFYEARS; i++) {
      for (j = 0; j < scores.xpoints()-1; j++) {
        switch (j) {
          case 0:
          case 5:
          case 6:
          case 7:
            if (fabs(allscores[i][j]) < fabs(allscores[bests[j]][j])) 
               bests[j] = i;
            break;
          case 3:
            if ((allscores[i][j]) > (allscores[bests[j]][j])) 
               bests[j] = i;
            break;
          default:
            if (fabs(allscores[i][j]) > fabs(allscores[bests[j]][j])) 
               bests[j] = i;
            break;
        }
      }
    }

//  Take each scorer:
    for (j = 0; j < scores.xpoints() - 1; j++) { 

//    check all leads:
      for (lead = 1; lead <= 12; lead++) {
        //  Get the actual field for the lead
        lseek(fin, (12*REFYEARS+month+lead)*sizeof(float)*land.xpoints()*
                   land.ypoints(), SEEK_SET);
        read(fin, obs.grid, sizeof(float)*obs.xpoints()*obs.ypoints() );
        if (obs.gridmax() > 3.0 ) {
          obs /= 100.;
          if (obs.gridmax() > 1.0 ) return 1;
        }
        
        // Get the analogous field at that lead:
        lseek(fin, (bests[j]*12 + month%12+lead)*sizeof(float)*obs.xpoints()*
              obs.ypoints(), SEEK_SET);
        read(fin, fcst.grid, sizeof(float)*obs.xpoints()*obs.ypoints() );
        if (fcst.gridmax() > 3.0 ) {
          fcst /= 100.;
          if (fcst.gridmax() > 1.0 ) return 1;
        }

        score2(fcst, obs, land, prob[month%12], scores);

        printf("%1d %3d %3d  %4.2f %4.2f %4.2f %4.2f  %4.2f  %6.3f %5.3f %5.3f  %5.3f\n",
               j, lead, month, scores[0], scores[1], scores[2], scores[3], 
               scores[4], scores[5], scores[6], scores[7], scores[8]);
       fflush(stdout);
      }
    }
    

  }


  return 0;
}
