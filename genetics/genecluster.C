#include <stdlib.h>
#include "ncepgrids.h"
#include "ssmiclass.h"
#include "genes.h"

void readin(mvector<mvector<int> > &y, ssmipt &tmax, ssmipt &tmin,
            int &count) ;
float scorer(mvector<mvector<int> > &y, mvector<united> &weights, int count) ;

void grazer(mvector<mvector<int> > &genomes, mvector<float> &scores, 
            float radius, genetic_code x) ;
float gene_distance(mvector<united> &weights, mvector<united> &test) ;

int main(int argc, char *argv[]) {
  mvector<mvector<int> > y(465*385*2*2);
  ssmipt tmax, tmin;
  int i, count = 0;
// Genetic part
  genetic_code x(8);            // 7 temperatures, 1 radius
  mvector<united> weights(8);
  mvector<mvector<int> > genome(POPULATION);
  mvector<float> scores(POPULATION);
  float best, mean = 0;
  int generation = 0, genmax = 200;
  united f1, f2;
  float similarity = SIMIL;

// Initialize the y mvector (note that it is oversized, and we'll be using 
//   'count' for the real measure.
  //printf("y.xpoints = %d\n",y.xpoints() ); fflush(stdout);
  for (i = 0; i < y.xpoints(); i++) {
    y[i].resize(7);
  }
  //printf("done resizing\n"); fflush(stdout);

// Read in the data and find its range
  readin(y, tmax, tmin, count);
  //printf("tmax: \n");
  //printf("%d %d %d %d %d %d %d\n",tmax.obs.t19v, tmax.obs.t19h, tmax.obs.t22v, 
//tmax.obs.t37v, tmax.obs.t37h, tmax.obs.t85v, tmax.obs.t85h);
  //printf("tmin: \n");
  //printf("%d %d %d %d %d %d %d\n",tmin.obs.t19v, tmin.obs.t19h, tmin.obs.t22v, 
//tmin.obs.t37v, tmin.obs.t37h, tmin.obs.t85v, tmin.obs.t85h);

  count -= 1;

// start random number generator
  if (argc > 1) {
    srand(atoi(argv[1]));
  }
  else {
    srand(1);
  }
  f1.ival = 138*100;
  f2.ival = 300*100;
  x.newgene(0, 14, INT_TYPE, f1, f2);
  x.newgene(1, 14, INT_TYPE, f1, f2);
  x.newgene(2, 14, INT_TYPE, f1, f2);
  x.newgene(3, 14, INT_TYPE, f1, f2);
  x.newgene(4, 14, INT_TYPE, f1, f2);
  x.newgene(5, 14, INT_TYPE, f1, f2);
  x.newgene(6, 14, INT_TYPE, f1, f2);
  f1.ival = 32;
  f2.ival = 2047;
  x.newgene(7, 11, INT_TYPE, f1, f2);
  //printf("about to set up the initial population\n"); fflush(stdout);
// Set up initial population:
  for (i = 0; i < POPULATION; i++) {
    genome[i].resize(x.code_length);
    newgenes(genome[i]);
    #ifdef VERBOSE
      transcriber(genome[i], weights, x);
      showgenes(stdout, genome[i], x); fflush(stdout);
    #endif
  }
  scores = (float) 0.;
  generation = 0;

  do {
    for (i = 0; i < POPULATION; i++) {
      if (scores[i] == 0.0) {
        transcriber(genome[i], weights, x);
        scores[i] = scorer(y, weights, count);
      }
    }

    best = scores.maximum();
    mean = scores.average();
    printf("generation %d scores: %f %f %f %f\n", generation,
       scores.maximum(),  scores.average(),  scores.rms(),  scores.norm(4) );
    fflush(stdout);

    order(genome, scores);
    reproducer(genome, scores);
    printf("\ngeneration %3d list\n", generation);
    for (i = 0; i < scores.xpoints()/4.; i++) {
      printf("score %f ",scores[i]);
      showgenes(stdout, genome[i], x);
      fflush(stdout);
    }
    printf("\n");

    grazer(genome, scores);
    grazer(genome, scores, similarity, x);
    generation += 1;

  } while ( ((best > 1.25*mean ) || (mean == 0.)) && (generation < genmax) );

  printf("final best, mean %f %f ratio %f generation %d\n",
                  best, mean, best/mean, generation);
  for (i = 0; i < genome.xpoints(); i++) {
    printf("%f :  ",scores[i]);
    showgenes(stdout, genome[i], x);
  }

  return 0;

}


float scorer(mvector<mvector<int> > &y, mvector<united> &weights, int count) {
  int recount = 0;
  mvector<int> tmp(7), center(7);
  int i;
  int radius = weights[weights.xpoints()-1].ival;
  float s0, sm, sp;

  for (i = 0; i < 7; i++) {
    center[i] = weights[i].ival;
  }

  for (i = 0; i < count; i++) {
    tmp = center;
    tmp -= y[i];
    if (tmp.norm() < radius) {
      recount += 1;
    }
  }
  s0 = 1.e6*(float) recount / ((4.*M_PI/3.)*radius*radius*radius);

//  radius /= 1.1;
//  recount = 0;
//  for (i = 0; i < count; i++) {
//    tmp = center;
//    tmp -= y[i];
//    if (tmp.norm() < radius) {
//      recount += 1;
//    }
//  }
//  sm = 1.e6*(float) recount / ((4.*M_PI/3.)*radius*radius*radius);
//
//  radius *= 1.1*1.1;
//  recount = 0;
//  for (i = 0; i < count; i++) {
//    tmp = center;
//    tmp -= y[i];
//    if (tmp.norm() < radius) {
//      recount += 1;
//    }
//  }
//  sp = 1.e6*(float) recount / ((4.*M_PI/3.)*radius*radius*radius);
//
//  printf("zz %e %e %e\n",sm, s0, sp);

  return s0;
}

void readin(mvector<mvector<int> > &y, ssmipt &tmax, ssmipt &tmin, 
            int &count) {
  FILE *fin;
  northgrid<ssmipt> x;
  ijpt loc;

  fin = fopen("n3ssmi.20040612","r");
  x.binin(fin);
  fclose(fin);

  tmax.obs.t19v = 0;
  tmax.obs.t19h = 0;
  tmax.obs.t22v = 0;
  tmax.obs.t37v = 0;
  tmax.obs.t37h = 0;
  tmax.obs.t85v = 0;
  tmax.obs.t85h = 0;
  tmin.obs.t19v = 50000;
  tmin.obs.t19h = 50000;
  tmin.obs.t22v = 50000;
  tmin.obs.t37v = 50000;
  tmin.obs.t37h = 50000;
  tmin.obs.t85v = 50000;
  tmin.obs.t85h = 50000;

  count = 0;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    tmax.obs.t19v = max(tmax.obs.t19v, x[loc].obs.t19v);
    tmax.obs.t19h = max(tmax.obs.t19h, x[loc].obs.t19h);
    tmax.obs.t22v = max(tmax.obs.t22v, x[loc].obs.t22v);
    tmax.obs.t37v = max(tmax.obs.t37v, x[loc].obs.t37v);
    tmax.obs.t37h = max(tmax.obs.t37h, x[loc].obs.t37h);
    tmax.obs.t85v = max(tmax.obs.t85v, x[loc].obs.t85v);
    tmax.obs.t85h = max(tmax.obs.t85h, x[loc].obs.t85h);
    if (x[loc].obs.t19v != 0) tmin.obs.t19v = min(tmin.obs.t19v, x[loc].obs.t19v);
    if (x[loc].obs.t19h != 0) tmin.obs.t19h = min(tmin.obs.t19h, x[loc].obs.t19h);
    if (x[loc].obs.t22v != 0) tmin.obs.t22v = min(tmin.obs.t22v, x[loc].obs.t22v);
    if (x[loc].obs.t37v != 0) tmin.obs.t37v = min(tmin.obs.t37v, x[loc].obs.t37v);
    if (x[loc].obs.t37h != 0) tmin.obs.t37h = min(tmin.obs.t37h, x[loc].obs.t37h);
    if (x[loc].obs.t85v != 0) tmin.obs.t85v = min(tmin.obs.t85v, x[loc].obs.t85v);
    if (x[loc].obs.t85h != 0) tmin.obs.t85h = min(tmin.obs.t85h, x[loc].obs.t85h);
// If a nonzero point, add to mvector
    if ( (x[loc].obs.t19v != 0) &&  (x[loc].obs.t19h != 0) &&  (x[loc].obs.t22v != 0) &&  (x[loc].obs.t37v != 0) &&  (x[loc].obs.t37h != 0) &&  (x[loc].obs.t85v != 0) &&  (x[loc].obs.t85h != 0) ) {
      count += 1;
      y[count][0] = x[loc].obs.t19v;
      y[count][1] = x[loc].obs.t19h;
      y[count][2] = x[loc].obs.t22v;
      y[count][3] = x[loc].obs.t37v;
      y[count][4] = x[loc].obs.t37h;
      y[count][5] = x[loc].obs.t85v;
      y[count][6] = x[loc].obs.t85h;
    }

  }
  }

  return;
}
///////////// Grazer 'eats up' the excessively identical genomes
void grazer(mvector<mvector<int> > &genomes, mvector<float> &scores, 
            float radius, genetic_code x) {
  int i, j;
  mvector<united> weights(8), test(8);
  float dist;

  for (i = 0; i < scores.xpoints(); i++) {
    transcriber(genomes[i],weights, x);
    for (j = i+1; j < scores.xpoints(); j++) {
      transcriber(genomes[j], test, x);
      dist = gene_distance(weights, test);
      //if (dist < 100*100) printf("dist = %f\n",dist); 
      //if (dist < weights[7].ival) {
      if (dist < radius) {
        //printf("grazing\n");
        if (scores[i] >= scores[j]) {
           mutate(genomes[j]); 
           scores[j] = 0.0;
        }
        else {
           mutate(genomes[i]); 
           scores[i] = 0.0;
        }
       continue;
      } // ifs testing on too-similar genomes
    }
  }
  return ;
}
float gene_distance(mvector<united> &weights, mvector<united> &test) {
  float sum = 0;
  int i;
  for (i = 0; i < 7; i++) {
     sum += (weights[i].ival - test[i].ival)*( weights[i].ival - test[i].ival);
  }
  return sqrt(sum);
}
