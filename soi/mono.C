#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "mvector.h"
#include "time_series.h"

#include "genes.h"

#define POPULATION 500


extern void soiread(time_series<float> &soi) ;

float scorer(mvector<united> &weights, time_series<float> &soi, int lead) ;
void displayer(mvector<int> &genome, time_series<float> &soi, int lead, genetic_code &x) ;

#define NGENES 5
#define LEADER 32

int main(int argc, char *argv[]) {
  genetic_code x(NGENES);
  mvector<mvector<int> > genome(POPULATION);
  mvector<mvector<int> > elite(POPULATION);
  mvector<united> weights;
  mvector<float> scores(POPULATION);
  time_series<float> soi(90*12);
  int i, j, lead;
  float best, mean;
  int generation = 0, genmax = 400;
  united f1, f2;

//Initialize:
  srand(1);

  soiread(soi);
  lead = atoi(argv[1]);

// Set up the genome
  f1.fval = -2.; f2.fval = 2.;
  x.newgene(0, 8, FLOAT_TYPE, f1, f2);
  x.newgene(1, 8, FLOAT_TYPE, f1, f2);

  f1.ival = 0; f2.ival = 15;
  x.newgene(2, 4, INT_TYPE, f1, f2);
  x.newgene(3, 4, INT_TYPE, f1, f2);
  x.newgene(4, 4, INT_TYPE, f1, f2);

  //printf("ncodes, code length = %d %d\n",x.ncodes, x.code_length);
   
  weights.resize(x.ncodes);

  for (i = 0; i < POPULATION; i++) {
     genome[i].resize(x.code_length);
     elite[i].resize(x.code_length);
     newgenes(genome[i]);
     //showgenes(stdout, genome[i], x);
  }

// Now start evaluating/evolving:
  scores = (float) 0.0;
  //printf("before loop, score max = %f\n",scores.maximum() );
  do {
    generation += 1;
    for (i = 0; i < POPULATION; i++) {
      if (scores[i] == 0.0) {
        transcriber(genome[i], weights, x);
        scores[i] = scorer(weights, soi, lead);
      } 
    }
    best = scores.maximum();
    mean = scores.average();
    printf("generation %d scores: %f %f %f %f\n", generation,
       scores.maximum(),  scores.average(),  scores.rms(),  scores.norm(4) );

    reproducer(genome, scores);
    order(genome, scores);
    grazer(genome, scores);
  } while (best > 1.125*mean && generation < genmax);

  printf("\n");
  for (i = 0; i < genome.xpoints(); i++) {
    printf("%f :  ",scores[i]);
    showgenes(stdout, genome[i], x);
  }
  displayer(genome[0], soi, lead, x);

  return 0;

}
/////////////////////////////////
void displayer(mvector<int> &genome, time_series<float> &soi, int lead, genetic_code &x) {
  float est, tmp_w;
  int i;
  mvector<united> weights(x.code_length);

  transcriber(genome, weights, x);
  for (i = LEADER; i < soi.xpoints() - lead ; i++) {
     est = soi[i-weights[2].ival]*weights[0].fval + 
            0.*weights[1].fval*soi[i-weights[3].ival]*soi[i-weights[4].ival] ;
     printf("%d  %f %f\n",i, est, soi[i+lead]);
  }
  return;
}
// Compute the score for a set of weights.  Ensure that it gives
//  a high, positive score for a good parameter set.
//  weights run back N time steps from current
float scorer(mvector<united> &weights, time_series<float> &soi, int lead) {
  float est, tmp_w = 0., tmp_avg = 0., score;
  float avg = 0;
  int i;

  for (i = LEADER; i < soi.xpoints()-lead; i++) {
    avg += soi[i+lead];
  }
  avg /= soi.xpoints()-lead-LEADER;

  for (i = LEADER; i < soi.xpoints()-lead; i++) {
    est = soi[i-weights[2].ival]*weights[0].fval +
           0.* weights[1].fval*soi[i-weights[3].ival]*soi[i-weights[4].ival] ;
    tmp_w   += (soi[i+lead] - est) * (soi[i+lead] - est);
    tmp_avg += (soi[i+lead] - avg) * (soi[i+lead] - avg);
  }
  score =  100.*(1. - tmp_w / tmp_avg ); // percent variance explained
  if (score > 100.) {
    printf("tmp_w = %f tmp_avg = %f\n",tmp_w, tmp_avg);
  }
  return score;
 
}

