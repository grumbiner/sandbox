#include "mvector.h"
#include "genes.h"

// set size for genetic code (# parameters),
//     set genmax, population size
void setup_genes(mvector<mvector<int> > &genome, mvector<united> &weights, mvector<float> &scores, genetic_code &gc, int &population, int &genmax) {

  gc.resize(21);
  weights.resize(gc.ncodes);

  genmax = 2000;    // these might be read in
  population = 200;
  genome.resize(population);
  scores.resize(population);

  united f1, f2;
  int i;

  f1.fval = -250; f2.fval = 250;
  for (i = 0; i < 7; i++) {
    gc.newgene(i, 9, FLOAT_TYPE, f1, f2);
  }
  f1.fval = -5; f2.fval = 5;
  for (i = 7; i < 14; i++) {
    gc.newgene(i, 10, FLOAT_TYPE, f1, f2);
  }
  f1.ival = 0; f2.ival=1;
  for (i = 14; i < 21; i++) {
    gc.newgene(i,1,INT_TYPE, f1, f2);
  }
  for (i = 0; i < population; i++) {
    genome[i].resize(gc.code_length);
    newgenes(genome[i]);
  }
  scores = (float) 0.;

  return;
}
