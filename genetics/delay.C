#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "ncepgrids.h"
#include "params.h"
#include "genes.h"

extern "C" float arcdis_(float &long1, float &lat1, float &long2, float &lat2 );

#define NDAY 365
// Program to experiment with training a genetic algorithm to
//   find teleconnections.
// Robert Grumbine 20 June 2003

#define POPULATION 5000

// Functions which are somewhat or very specialized towards the
// problem at hand.
float scorer(GRIDTYPE<float> *sst, mvector<united> &weights) ;

void get_data(char *fname, char *climo, GRIDTYPE<float> *sst);
void delta(GRIDTYPE<float> &sst, GRIDTYPE<float> &climo) ;

////////////////// Main program ///////////////
int main(int argc, char *argv[]) {
//Gene-related:
  genetic_code x(5);
  mvector<united> weights;
  mvector<mvector<int> > genome(POPULATION);
  mvector<float> scores(POPULATION);
  float best, mean = 0.;
  int generation = 0, genmax ;

// Problem-related:
  GRIDTYPE<float> sst[NDAY];
  int i;
  united f1, f2;
  FILE *fout;

// Utility:
  printf("argc = %d\n", argc); 
  for (i = 0; i < argc; i++) {
    printf("%s\n",argv[i]);
  }
  genmax = atoi(argv[4]);

///// Read in the model data 
  get_data(argv[1], argv[2], sst);

//Get critters, using _de_novo_ initialization of genetics if necessary:

  srand(1);

  f1.ival = 0; f2.ival = sst[0].xpoints();
  x.newgene(0, 10, INT_TYPE, f1, f2);
  x.newgene(2, 10, INT_TYPE, f1, f2);

  //f1.ival = 4; f2.ival = 310; //For sst, exclude the South pole and 
  //                            //  the Arctic perennial ice pack
  f1.ival = 0; f2.ival = sst[0].ypoints(); 
  x.newgene(1, 9, INT_TYPE, f1, f2);
  x.newgene(3, 9, INT_TYPE, f1, f2);
  f1.ival = 16; f2.ival = 31;
  x.newgene(4, 4, INT_TYPE, f1, f2);
 
  weights.resize(x.ncodes);

  for (i = 0; i < POPULATION; i++) {
     genome[i].resize(x.code_length);
     newgenes(genome[i]);
  }

////////////////////////////////////////////////////////////////////////////

  scores = (float) 0.;
  generation = 0;

  do {
    for (i = 0; i < POPULATION; i++) {
      if (scores[i] == 0.0) {
        transcriber(genome[i], weights, x);
        scores[i] = scorer(sst, weights);
        printf("trial %f ", scores[i]); fflush(stdout);
        showgenes(stdout, genome[i], x); fflush(stdout);
      } 
    }
    best = scores.maximum();
    mean = scores.average();
    printf("epoch %d generation %d scores: %f %f %f %f\n", 0, generation,
       scores.maximum(),  scores.average(),  scores.rms(),  scores.norm(4) );
    fflush(stdout);

    reproducer(genome, scores);
    order(genome, scores);

    printf("\ngeneration %3d top 15 list\n", generation);
    for (i = 0; i < 15; i++) { 
      printf("score %f ",scores[i]);
      showgenes(stdout, genome[i], x);
      fflush(stdout);
    }
    printf("\n");

    grazer(genome, scores);
    generation += 1;

  } while ( (best > (1.+1./5.)*mean || best < 1./3.) && 
               generation < genmax );

  printf("best, mean %f %f ratio %f generation %d\n",best, mean, 
                        best/mean, generation);

  for (i = 0; i < genome.xpoints(); i++) {
    printf("%f :  ",scores[i]);
    showgenes(stdout, genome[i], x);
  }


////////////////////////////////////////
// For all run types, print out the genetic code and the genomes 
  fout = fopen(argv[3], "w"); 
  x.write(fout);
  for (i = 0; i < genome.xpoints(); i++) {
    fprintf(fout,"%f :  ",scores[i]);
    showgenes(fout, genome[i], x);
  }
  fclose(fout);

  return 0;

}

/////////////////////////////////
// Compute the score for a set of weights.  Ensure that it gives
//  a high, positive score for a good parameter set.

float scorer(GRIDTYPE<float> *sst, mvector<united> &weights) {
  ijpt loc1, loc2;
  float sumxy = 0., sumx2 = 0., sumy2 = 0., score;
  float dist;
  latpt ll1, ll2;
  int i;

  loc1.i = weights[0].ival;
  loc1.j = weights[1].ival;
  loc2.i = weights[2].ival;
  loc2.j = weights[3].ival;

  for (i = 0; i < NDAY; i++) {
    sumxy += sst[i][loc1] * sst[(i+weights[4].ival)%NDAY][loc2];
    sumx2 += sst[i][loc1] * sst[i][loc1];
    sumy2 += sst[(i+weights[4].ival)%NDAY][loc2] * 
             sst[(i+weights[4].ival)%NDAY][loc2];
  }
  if (sumx2 != 0. && sumy2 != 0.) {
    score = sumxy / sqrt(sumx2*sumy2);
    score *= score;
  }
  else {
    score = 0.0;
  }

// Lead time predictors.
//  if (weights[4].ival == 0) {
//    return score;
//  }
//  else {
//    return pow(score, 7./weights[4].ival);
//  }
  //return score;
  return score*sumy2;
//Versions for concurrent
  //return score ;
  //return score*sumy2;
  //return score * fabs((float)(loc2.j - loc1.j));
  //ll1 = sst[0].locate(loc1);
  //ll2 = sst[0].locate(loc2);
  //dist = arcdis_(ll1.lon, ll1.lat, ll2.lon, ll2.lat);
  //return score * dist/1.e3;
}

/////////////////////////////////
//  Read in the data -- required for all versions of the program:
void get_data(char *fname, char *climo, GRIDTYPE<float> *sst) {
  FILE *sstin, *climin;
  int i;
  GRIDTYPE<float> avger, clim;
  
  sstin = fopen(fname, "r");
  if (sstin == (FILE *) NULL ){
    printf("failed to open %s file\n",fname);
    exit(2);
  }
  climin = fopen(climo, "r");
  if (climin == (FILE *) NULL ){
    printf("failed to open %s file\n",fname);
    exit(2);
  }
  
  avger.set((float) 0.0);
  for (i = 0; i < NDAY; i++) {
    clim.binin(climin);
    sst[i].binin(sstin);
    delta(sst[i], clim); //sst[i] -= clim;
    avger += sst[i];
  }
  fclose(sstin);
  avger /= (float) NDAY;
  for (i = 0; i < NDAY; i++) {
    sst[i] -= avger;
  }

  printf("annual average %f\n",avger.average() );
  for (i = 0; i < NDAY; i++) {
    printf("%3d avg  %f  rms %f\n",i, sst[i].average(), sst[i].rms() );
  }
  return;
}
void delta(GRIDTYPE<float> &sst, GRIDTYPE<float> &climo) {
  ijpt loc;
  if (sst.average() > 3.0) sst /= 100.;
  if (climo.average() > 3.0) climo /= 100.;

  for (loc.j = 0; loc.j < sst.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < sst.xpoints(); loc.i++) {
    if (sst[loc] > 1.28 || climo[loc] > 1.28) {
      sst[loc] = 0.0;
    }
    else {
      sst[loc] -= climo[loc];
    } 
  }
  }

  return;

}
