#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include "mvector.h"
#include "time_series.h"

#include "collect.C"

#define GENES 4
#define BITS  32
#define POPULATION 500
#define PCROSS 0.75
#define PMUTATE (1./GENES/BITS)
#define LEAD 1


extern void soiread(time_series<float> &soi) ;

void transcriber(mvector<int> &genome, mvector<float> &weights) ;
void showgenes(FILE *fout, mvector<int> &genome) ;
void newgenes(mvector<int> &genome) ;
void reproducer(mvector<mvector<int> > &genomes, mvector<float> &scores) ;
float scorer(mvector<float> &weights, time_series<float> &soi) ;
void displayer(mvector<int> &genome, time_series<float> &soi) ;



int main(void) {
  //mvector<int> genome(GENES*(BITS+1));
  mvector<mvector<int> > genome(POPULATION);
  mvector<mvector<int> > elite(POPULATION);
  mvector<float> weights(GENES), scores(POPULATION), elite_scores(POPULATION);
  time_series<float> soi(90*12);
  int i, j;
  float best, mean;
  int generation = 0, genmax = 400;

//Initialize:
  srand(1);

  soiread(soi);
  for (i = 0; i < soi.xpoints(); i++) {
    soi[i] = sin(2.*M_PI*i/7.5);
  }

  for (i = 0; i < POPULATION; i++) {
     genome[i].resize(GENES*(BITS+1));
     elite[i].resize(GENES*(BITS+1));
     newgenes(genome[i]);
  }

// Now start evaluating/evolving:
  scores = (float) 0.0;
  elite_scores = (float) 0.0;
  do {
    generation += 1;
    for (i = 0; i < POPULATION; i++) {
      if (scores[i] == 0.0) {
        transcriber(genome[i], weights);
        scores[i] = scorer(weights, soi);
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
    showgenes(stdout, genome[i]);
  }
  displayer(genome[0], soi);


  return 0;

}
void displayer(mvector<int> &genome, time_series<float> &soi) {
  float est, tmp_w;
  int i, j, lead = LEAD;
  mvector<float> weights(GENES);
  transcriber(genome, weights);
  for (i = weights.xpoints(); i < soi.xpoints() - lead -1; i++) {
     est = 0.0;
     for (j = 0; j < weights.xpoints(); j++) {
        est += soi[i-j]*weights[j];
     }
     printf("%d  %f %f\n",i, est, soi[i+lead]);
  }
  return;
}
/////////////////////////////////
// Compute the score for a set of weights.  Ensure that it gives
//  a high, positive score for a good parameter set.
//  weights run back N time steps from current
float scorer(mvector<float> &weights, time_series<float> &soi) {
  float est, tmp_w = 0., tmp_avg = 0., sumw = 0.;
  float avg = soi.average();
  int i, j;
  int lead=LEAD;

  for (j = 0; j < weights.xpoints(); j++) {
    sumw += weights[j];
  }
  if (sumw == 0.) sumw = 1.;
  sumw = 1.;  //ignore the above
  for (i = weights.xpoints(); i < soi.xpoints() - 1 - lead; i++) {
    est = 0.0;
    for (j = 0; j < weights.xpoints(); j++) {
      est += soi[i-j] * weights[j];
    }
    est /= sumw;
    tmp_w   += (soi[i+lead] - est) * (soi[i+lead] - est);
    tmp_avg += (soi[i+lead] - avg) * (soi[i+lead] - avg);
  }
  return 100.*(1. - tmp_w / tmp_avg ); // percent variance explained
  //return tmp_avg / tmp_w;
 
}

///////////////////////////////////////////
// Transcribe a hierarchal genome into a mvector of floating point weights
//   determine number of bits per gene from fact that number of weights
//   will be given
void transcriber(mvector<int> &genome, mvector<float> &weights) {
  int ngenes = weights.xpoints();
  int bits   = genome.xpoints()/weights.xpoints() - 1;

  int i, j, mul, base, tmp;
  float val;

  for (i = 0; i < ngenes; i++) {
    if (genome[i] == 0 ) {
      weights[i] = 0.0;
    }
    else {
      base = ngenes + i*bits;
      if (genome[base] == 1) { mul = -1; }
      else {
        mul = 1;
      } 
      tmp = 0;
      for (j = base+1; j < base + bits; j++) {
        tmp += mul*genome[j];
        mul *= 2;  
      }
      // +-1 weights[i] = (float) tmp / (pow(2., (float)bits-1) - 1.);
      // +-2 
      weights[i] = (float) tmp / (pow(2., (float)bits-2) );
      // +-4 weights[i] = (float) tmp / (pow(2., (float)bits-3) );
    }
  }

  return ;
}
void reproducer(mvector<mvector<int> > &genomes, mvector<float> &scores) {
  float average = scores.average(), reqt;
  int i, j, nbetter;
  float total, running, trand;
  int nparents = 0, parent1, parent2;
  int iter = 0, tries;
  mvector<float> pcross(scores.xpoints() );

  nbetter = 0;
  for (i = 0; i < genomes.xpoints(); i++) {
    if (scores[i] > average) nbetter += 1;
  }
  printf("%d genomes of %d above average  ",nbetter, genomes.xpoints() );

  if (nbetter > genomes.xpoints()/2) {
    reqt = average + 0.25 * (scores.maximum() - average) ;
  }
  else {
    reqt = average;
  }
  reqt = max(-100.0, reqt);
// Weight roulette wheel by score, high score is better
  total = 0.0;
  for (i = 0; i < genomes.xpoints(); i++) {
    if (scores[i] > reqt) { total += scores[i]; }
    else {
      scores[i] = 0;
    }
  }
  running = 0.;
  for (i = 0; i < genomes.xpoints(); i++) {
    if (scores[i] > 0.0) {
      nparents += 1;
      running += scores[i] / total;
      pcross[i] = running;
    }
  }

  if (nparents != 1) {
    for (i = 0; i < genomes.xpoints(); i++) {
      if (scores[i] == 0. ) {
        if ((1.0*rand()) /(RAND_MAX+1.0) < PCROSS ) {
          trand = (1.0*rand()) /(RAND_MAX+1.0);
          parent1 = find_parent(pcross, trand);
          tries = 0;
          do {
            tries += 1;
            trand = (1.0*rand()) /(RAND_MAX+1.0);
            parent2 = find_parent(pcross, trand);
            //printf("parent2 parent1 %d %d %d\n", parent2, parent1, nparents);
          } while (parent2 == parent1 && tries < 10);
          crossover(genomes, parent1, parent2, i);
        }
        else {
          newgenes(genomes[i]);
        }
      }
    }
  }
  // end of horizontal transfer/crossover.  Skip if only 1 parent

  // apply mutation:
  for (i = 0; i < genomes.xpoints(); i++) {
    if (scores[i] <= reqt) {
      mutate(genomes[i]);
      // Note that we don't have to worry about lethal mutations
    }
  }

  return;

}


/////////////////////////////////
void newgenes(mvector<int> &genome) {
  int i;
  for (i = 0; i < genome.xpoints(); i++) {
    genome[i] = (int) (0.5 + (1.0*rand()) /(RAND_MAX+1.0) );
  }
  return;
}
void showgenes(FILE *fout, mvector<int> &genome) {
  int i;
  mvector<float> weights(GENES);
  transcriber(genome, weights);
  for (i = 0; i < weights.xpoints(); i++) {
    //fprintf(fout, "%d  %1d %8.5f  ",i, genome[i], weights[i]);   
    fprintf(fout, "%8.5f  ",weights[i]);   
  }
  fprintf(fout, "\n");
  return;
}

/////////////////////////
