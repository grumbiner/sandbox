#include "mvector.h"

#ifndef COLLECT_H
  #define COLLECT_H
#define GENES 2
#define BITS  14
#define POPULATION 250
#define PCROSS 0.5
#define PMUTATE (1./GENES/BITS)

// Generic genetics
void swap(mvector<int> &x, mvector<int> &y) ;
void order(mvector<mvector<int> > &genomes, mvector<float> &scores);
void grazer(mvector<mvector<int> > &genomes, mvector<float> &scores) ;
void mutate(mvector<int> &genome) ;
int find_parent(mvector<float> &table, float trand) ;
int crossover(mvector<mvector<int> > &genomes, int p1, int p2, int rep) ;

/////////////////////////
//Swap two genomes:
void swap(mvector<int> &x, mvector<int> &y) {
  mvector<int> tmp(x.xpoints() );

  tmp = y;
  y = x;
  x = tmp;

  return ;
}
/////////////////////////
// Partial sorting of genomes by score:
void order(mvector<mvector<int> > &genomes, mvector<float> &scores) {
  int i, j;
  float tmp;

  for (i = 0; i < scores.xpoints() - 1; i++) {
    for (j = i+1; j < scores.xpoints() ; j++) {
      if (scores[j] > scores[i]) {
        tmp = scores[j]; scores[j] = scores[i]; scores[i] = tmp;
        swap(genomes[i], genomes[j]);
        continue;
      }
    }
  }

  return;
}
///////////// Grazer 'eats up' the excessively identical genomes
void grazer(mvector<mvector<int> > &genomes, mvector<float> &scores) {
  int i, j;

  for (i = 0; i < scores.xpoints(); i++) {
    for (j = i+1; j < scores.xpoints(); j++) {
       if (scores[i] == scores[j]) { // might be same genome
         if (genomes[i] == genomes[j]) {
           mutate(genomes[j]);
           scores[j] = 0.0;
           continue;
         }
       } // ifs testing on too-similar genomes
    }
  }
  return ;
}
void mutate(mvector<int> &genome) {
  int j;

  for (j = 0; j < genome.xpoints(); j++) {
    if ((1.0*rand()) /(RAND_MAX+1.0) < PMUTATE) {
      if (genome[j] == 1) { genome[j] = 0; }
      else { genome[j] = 1 ; }
    }
  }
  return;
}
int find_parent(mvector<float> &table, float trand) {
  int i;
  for (i = 0; i < table.xpoints(); i++) {
    if (trand < table[i]) return i;
  }
  return 0;
}
int crossover(mvector<mvector<int> > &genomes, int p1, int p2, int rep) {
  int crosspoint, i;
  crosspoint = (int) (0.5 + ( (float) genomes[p1].xpoints()*rand())/
                             (RAND_MAX + 1.0) );
  for (i = 0; i < genomes[p1].xpoints(); i++) {
    if (i < crosspoint) {
      genomes[rep][i] = genomes[p1][i];
    }
    else {
      genomes[rep][i] = genomes[p2][i];
    }
  }
  return crosspoint;
}

#endif
