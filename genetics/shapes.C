#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "float.h"
#include "genes.h"
#ifndef PARAMETERS
  #include "params.h"
#endif
#include "ncepgrids.h"

// Program to experiment with training a genetic algorithm to
//   fit shape to a data file
// Robert Grumbine 10 July 2004 -- Initial Version

#define POPULATION 1000

// Functions which are somewhat or very specialized towards the
// problem at hand.
float scorer(metricgrid<float> &sst, mvector<united> &weights) ;

////////////////// Main program ///////////////
int main(int argc, char *argv[]) {
//Gene-related:
  genetic_code x(3);
// These gene-related points don't change much, if at all
  mvector<united> weights;
  mvector<mvector<int> > genome(POPULATION);
  mvector<float> scores(POPULATION);
  float best, mean = 0.;
  int generation = 0, genmax = 20000;

// Problem-related:
  global_ice<float> sst;
  int i;
  united f1, f2;
  FILE *fin;

// Utility:
  printf("argc = %d\n", argc); 
  for (i = 0; i < argc; i++) {
    printf("%s\n",argv[i]);
  }

  fin = fopen(argv[1],"r");
  sst.binin(fin);
  printf("max, min sst %f %f\n",sst.gridmax(), sst.gridmin() );
 
  srand(1);
  
// Shape has i,j, magnitude trio
  f1.ival = 0; 
  f2.ival = sst.xpoints() - 1;
  x.newgene(0, 10, INT_TYPE, f1, f2);

  f2.ival = sst.ypoints() - 1;
  x.newgene(1, 9, INT_TYPE, f1, f2);
  f2.ival = min(sst.xpoints() , sst.ypoints() ) ;
  x.newgene(2, 9, INT_TYPE, f1, f2);
   
  weights.resize(x.ncodes);
  #ifdef VERBOSE
    printf("code length = %d\n",x.code_length); fflush(stdout);
  #endif

  for (i = 0; i < POPULATION; i++) {
     genome[i].resize(x.code_length);
     newgenes(genome[i]);
     #ifdef VERBOSE
       transcriber(genome[i], weights, x);
       showgenes(stdout, genome[i], x); fflush(stdout);
     #endif
  }

////////////////////////////////////////////////////////////////////////////
    scores = (float) 0.;
    generation = 0;

    do {
      #ifdef VERBOSE
        printf("in while loop, generation = %d\n",generation); fflush(stdout);
        fflush(stdout);
      #endif
      for (i = 0; i < POPULATION; i++) {
        #ifdef VERBOSE
          printf("in do loop, i = %d\n",i); fflush(stdout);
          fflush(stdout);
        #endif
        if (scores[i] == 0.0) {
          transcriber(genome[i], weights, x);
          scores[i] = scorer(sst, weights) ;
          //printf("trial %f ", scores[i]); fflush(stdout);
          //showgenes(stdout, genome[i], x); fflush(stdout);
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

  return 0;

}

/////////////////////////////////
// Compute the score for a set of weights.  Ensure that it gives
//  a high, positive score for a good parameter set.

float scorer(metricgrid<float> &sst, mvector<united> &weights) {
  ijpt loc, tloc;
  float score;
  float a = 0., sx = 0., sx2 = 0.;

  for (loc.j = weights[1].ival; 
       loc.j < min(sst.ypoints(), weights[1].ival + weights[2].ival); 
       loc.j++) {
  tloc.j = loc.j;
  for (loc.i = weights[0].ival; 
       loc.i < weights[0].ival + weights[2].ival ; 
       loc.i++) {
    tloc.i = loc.i % sst.xpoints();
    a   += 1.0;
    sx  += sst[tloc];
    sx2 += sst[tloc]*sst[tloc]; 
  }
  }

  if (a > 1) {
    sx2 /= a;
    sx  /= a;
    score = sx2 - sx*sx;
    if (score < 0) score = 0;
  }
  else {
    score = 0.;
  }
 
  //printf("score %f %f %f  %f\n",a, sx, sx2, score);

  if (score != 0.) {
    return a / sqrt(score);
  }
  else {
    return 0.;
  }

}
