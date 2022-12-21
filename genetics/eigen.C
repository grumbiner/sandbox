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

#define POPULATION 500

// Functions which are somewhat or very specialized towards the
// problem at hand.
float scorer(mvector<united> &weights) ;
float eigenvalue(mvector<united> &weights) ;

////////////////// Main program ///////////////
int main(int argc, char *argv[]) {
//Gene-related:
  genetic_code x(10);
// These gene-related points don't change much, if at all
  mvector<united> weights;
  mvector<mvector<int> > genome(POPULATION);
  mvector<float> scores(POPULATION);
  float best, mean = 0.;
  int generation = 0, genmax = 4000*60;
  int i;
  united f1, f2;

// Problem-related:
  // Will compute u*u_x - u_xx vs. L*u

// Utility:
  printf("argc = %d\n", argc); 
  for (i = 0; i < argc; i++) {
    printf("%s\n",argv[i]);
  }

  srand(1);
  
// The genes are values of u in the operator.  We'll assume they're
// fairly well scaled.
  f1.fval = -2.0;
  f2.fval =  2.0;
  for (i = 0; i < x.ncodes; i++) {
    x.newgene(i, 9, FLOAT_TYPE, f1, f2);
  }
 
  weights.resize(x.ncodes);

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
          scores[i] = scorer(weights) ;
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
      for (i = 0; i < 1; i++) { 
          transcriber(genome[i], weights, x);
        printf("score %e %e:",scores[i], eigenvalue(weights) );
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
      transcriber(genome[i], weights, x);
      printf("%f %f:  ",scores[i],eigenvalue(weights) );
      showgenes(stdout, genome[i], x);
    }

  return 0;

}

/////////////////////////////////
// Compute the score for a set of weights.  Ensure that it gives
//  a high, positive score for a good parameter set.
float scorer(mvector<united> &weights) {
  // linear function -- u_xx + Lu = 0
  float score = 0.0, lamda_avg = 0.0;
  float diff, slope, x, t;
  int i, nx = weights.xpoints();
  float eigen_ref = 6.0;

// Find the average eigenvalue:
  for (i = 1; i < nx - 1; i++) {
    x = (float)i;
    x /= (float) nx;
    diff = weights[i+1].fval - 2.*weights[i].fval + weights[i-1].fval;
    slope = (weights[i+1].fval - weights[i-1].fval)/2.;
    if (weights[i].fval != 0.0) {
      t =  (diff*(1.-x*x) - 2.*x*slope)/weights[i].fval;
      lamda_avg += t;
    }
  }
// Manage boundary conditions here.  Legendre is [0,1]
  i = 0;
  x = (float)i;
  x /= (float) nx;
  diff = weights[i+2].fval - 2.*weights[i+1].fval + weights[i].fval;
  slope = (weights[i+1].fval - weights[i].fval);
  if (weights[i].fval != 0.0) {
    t =  (diff*(1.-x*x) - 2.*x*slope)/weights[i].fval;
    lamda_avg += t;
  }
  i = nx -1;
  x = (float)i;
  x /= (float) nx;
  diff = weights[i].fval - 2.*weights[i-1].fval + weights[i-2].fval;
  slope = (weights[i].fval - weights[i-1].fval);
  if (weights[i].fval != 0.0) {
    t =  (diff*(1.-x*x) - 2.*x*slope)/weights[i].fval;
    lamda_avg += t;
  }
  lamda_avg /= (float) nx;
  lamda_avg -= eigen_ref;

// Find the mean squared deviation:
  for (i = 1; i < nx - 1; i++) {
    diff = weights[i+1].fval - 2.*weights[i].fval + weights[i-1].fval;
    slope = (weights[i+1].fval - weights[i-1].fval)/2.;
    if (weights[i].fval != 0.0) {
      score += (lamda_avg - (diff*(1.-x*x) - 2.*x*slope)/weights[i].fval)*
               (lamda_avg - (diff*(1.-x*x) - 2.*x*slope)/weights[i].fval);
    }
  }
  i = 0;
  diff = weights[i+1].fval - 2.*weights[i].fval + weights[nx-1].fval;
  slope = (weights[i+1].fval - weights[i].fval);
  if (weights[i].fval != 0.0) {
      score += (lamda_avg - (diff*(1.-x*x) - 2.*x*slope)/weights[i].fval)*
               (lamda_avg - (diff*(1.-x*x) - 2.*x*slope)/weights[i].fval);
  }
  i = nx -1;
  diff = weights[nx-1].fval - 2.*weights[nx-2].fval + weights[nx-3].fval;
  slope = (weights[i].fval - weights[i-1].fval);
  if (weights[i].fval != 0.0) {
      score += (lamda_avg - (diff*(1.-x*x) - 2.*x*slope)/weights[i].fval)*
               (lamda_avg - (diff*(1.-x*x) - 2.*x*slope)/weights[i].fval);
  }

  //printf("scorer %e %e\n",lamda_avg, score);

  if (score != 0.0) {
    score = 1./score;
  }
  return score;

}
/////////////////////////////////
// Compute the score for a set of weights.  Ensure that it gives
//  a high, positive score for a good parameter set.
float scorer_nonlinear(mvector<united> &weights) {
  float score = 0.0, lamda_avg = 0.0;
  float adv, diff;
  int i, nx = weights.xpoints();

  for (i = 1; i < nx - 1; i++) {
    adv = weights[i].fval*(weights[i+1].fval - weights[i-1].fval);
    diff = weights[i+1].fval - 2.*weights[i].fval + weights[i-1].fval;
    if (weights[i].fval != 0.0) {
      lamda_avg += (adv - diff)/weights[i].fval;
    }
  }
  i = 0;
  adv = weights[i].fval*(weights[i+1].fval - weights[nx-1].fval);
  diff = weights[i+1].fval - 2.*weights[i].fval + weights[nx-1].fval;
  if (weights[i].fval != 0.0) {
    lamda_avg += (adv - diff)/weights[i].fval;
  }
  i = nx -1;
  adv = weights[i].fval*(weights[0].fval - weights[i-1].fval);
  diff = weights[0].fval - 2.*weights[i].fval + weights[i-1].fval;
  if (weights[i].fval != 0.0) {
    lamda_avg += (adv - diff)/weights[i].fval;
  }
  lamda_avg /= (float) nx;

  for (i = 1; i < nx - 1; i++) {
    adv = weights[i].fval*(weights[i+1].fval - weights[i-1].fval);
    diff = weights[i+1].fval - 2.*weights[i].fval + weights[i-1].fval;
    if (weights[i].fval != 0.0) {
      score += (lamda_avg - (adv - diff)/weights[i].fval)*(lamda_avg - (adv - diff)/weights[i].fval);
    }
  }
  i = 0;
  adv = weights[i].fval*(weights[i+1].fval - weights[nx-1].fval);
  diff = weights[i+1].fval - 2.*weights[i].fval + weights[nx-1].fval;
  if (weights[i].fval != 0.0) {
      score += (lamda_avg - (adv - diff)/weights[i].fval)*
               (lamda_avg - (adv - diff)/weights[i].fval);
  }
  i = nx -1;
  adv = weights[i].fval*(weights[0].fval - weights[i-1].fval);
  diff = weights[0].fval - 2.*weights[i].fval + weights[i-1].fval;
  if (weights[i].fval != 0.0) {
      score += (lamda_avg - (adv - diff)/weights[i].fval)*
               (lamda_avg - (adv - diff)/weights[i].fval);
  }

  //printf("scorer %e %e\n",lamda_avg, score);

  if (score != 0.0) {
    score = lamda_avg*lamda_avg/score;
  }
  return score;

}
float eigenvalue(mvector<united> &weights) {
  // linear function -- u_xx + Lu = 0
  float score = 0.0, lamda_avg = 0.0;
  float diff, slope, x, t;
  int i, nx = weights.xpoints();
                                                                                 
  for (i = 1; i < nx - 1; i++) {
    x = (float)i;
    x /= (float) nx;
    diff = weights[i+1].fval - 2.*weights[i].fval + weights[i-1].fval;
    slope = (weights[i+1].fval - weights[i-1].fval)/2.;
    if (weights[i].fval != 0.0) {
      t =  (diff*(1.-x*x) - 2.*x*slope)/weights[i].fval;
      lamda_avg += t;
    }
  }
// Manage boundary conditions here.  Legendre is [0,1]
  i = 0; 
  x = (float)i;
  x /= (float) nx;
  diff = weights[i+2].fval - 2.*weights[i+1].fval + weights[i].fval;
  slope = (weights[i+1].fval - weights[i].fval);
  if (weights[i].fval != 0.0) {
    t =  (diff*(1.-x*x) - 2.*x*slope)/weights[i].fval;
    lamda_avg += t;
  }
  i = nx -1;
  x = (float)i;
  x /= (float) nx;
  diff = weights[i].fval - 2.*weights[i-1].fval + weights[i-2].fval;
  slope = (weights[i].fval - weights[i-1].fval);
  if (weights[i].fval != 0.0) {
    t =  (diff*(1.-x*x) - 2.*x*slope)/weights[i].fval;
    lamda_avg += t;
  }
 
  return (lamda_avg/nx);
}
