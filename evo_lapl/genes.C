#include "mvector.h"
#include "genes.h"

// Universal (?) header for doing evolution
//
void setup_genes(mvector<mvector<int> > &genome, mvector<united> &weights, mvector<float> &scores, genetic_code &gc, int &population, int &genmax);
float runscore(mvector<united> &weights);

int main(int argc, char *argv[]) {
// Declarations for evolutionary components:
  genetic_code gc;
  mvector<united> weights;
  mvector<mvector<int> > genome;
  mvector<float> scores;
  united f1, f2;

  float best, mean = 0;
  int generation = 0, genmax, population;
  int i;

  // set size for genetic code (# parameters),
  //     set genmax, population size
  setup_genes(genome, weights, scores, gc, population, genmax);

  // get data to be working with -- must be shared with scorer
    
// Conduct the evolution:
  generation = 0;
  do {
    for (i = 0; i < population; i++) {
      transcriber(genome[i], weights, gc);

// in to 'runscore' -- parsing out genes/weights, finding score, ..
      scores[i] = runscore(weights);
//
    } 
    best = scores.maximum();
    mean = scores.average();
    printf(" generation %4d stats %5.3f %5.3f\n",generation, best, mean); fflush(stdout);

    reproducer(genome, scores);
    order(genome, scores);

    if ((generation % 1) == 0) {
      fprintf(stdout, "\ngeneration %4d top 15 list\n", generation);
      for (i = 0; i < 15; i++) {
        fprintf(stdout, "score %f ",scores[i]);
        showgenes(stdout, genome[i], gc); fflush(stdout);
      }
      fprintf(stdout,"\n");
    }

    grazer(genome, scores);
    generation += 1;
  } while (generation < genmax);

  printf("best, score = %f\n",scores[0]);
  transcriber(genome[0], weights, gc);
  printf("weights:\n");
  for (i = 0; i < 7; i++) {
    printf("%7.2f  %6.3f %1d\n",weights[i].fval, weights[i+7].fval, weights[i+14].ival);
  }

  return 0;
}
