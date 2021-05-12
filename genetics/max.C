#include <stdlib.h>
#include "ncepgrids.h"
#include "genes.h"

// Experiment with the population size -- either by uncommenting and editing
// the following line, or by putting it in to a compile line as
//   -DPOPULATION=500 (or whatever your value is)
// #define POPULATION 500

// Global variable to track all the evaluations you call for
int evaluations = 0;

// Scoring function -- you always have to write this
float scorer(global_ice<float> &sst, mvector<united> &weights) ;


int main(int argc, char *argv[]) {
// Gene-related:
  genetic_code x(2); // 2 terms -- i and j coordinates
  mvector<united> weights;  // universal line -- will always have a vector
                            // of weights.
  mvector<mvector<int> > genome(POPULATION); // we have a population of genomes
  mvector<float> scores(POPULATION);       // .. and each population member
                                           // has a score
  float best, mean = 0.; // track how we're doing
  int generation = 0, genmax = 200; // count the generations, and limit to 200

// Problem-related:
  FILE *fin;
  global_ice<float> sst;
  united f1, f2;

// utility variable
  int i;

  // Initialize the random number generator.  If you don't specify a number,
  // we'll default to 1
  if (argc > 1) {
    srand(atoi(argv[1]));
  }
  else {
    srand(1);
  }
// Set up the genetic_code:
  // Note that C runs from 0 to N-1
  // First genetic code is the integer (.ival) which represents the i coordinate
  //  f1 is the lower bound, f2 is the upper bound
  f1.ival = 0;
  f2.ival = sst.xpoints() - 1;
  // Gene number 0 has 9 bits, is of INT_TYPE (could have been FLOAT_TYPE), and
  //   ranges from f1 to f2.  10 bits lets the code go to 1023.  sst.xpoints()-1
  //   is actually 719.  The MMAB library will handle keeping the numbers in
  //   bounds
  x.newgene(0, 10, INT_TYPE, f1, f2);
  // Now for the j direction.  Since the max j is 359, we only need 9 bits
  f1.ival = 0;
  f2.ival = sst.ypoints() - 1;
  x.newgene(1, 9, INT_TYPE, f1, f2);
  weights.resize(x.ncodes);

// Universal -- make space for the genetic codes now that we know what
//    they look like.  
// Also, randomly initialize the codes.  
// If compiled with -DVERBOSE, show what the codes look like (after
//   translation according the the preceding definition).
  for (i = 0; i < POPULATION; i++) {
    genome[i].resize(x.code_length);
    newgenes(genome[i]);
    #ifdef VERBOSE
      transcriber(genome[i], weights, x);
      showgenes(stdout, genome[i], x); fflush(stdout);
    #endif
  }

// Open the data file and read it in
  fin = fopen("sst","r");
  sst.binin(fin);
  fclose(fin);
  // You could subtract off the minimum or average value by uncommenting 
  // one of the next two lines:
  // sst -= sst.gridmin();
  // sst -= sst.average();

  printf("Diagnostic -- %f maximum as found by exhaustive search, %d evaluations\n",
          sst.gridmax(), sst.xpoints()*sst.ypoints() );

// Initialize scores and generation number:
  scores = (float) 0.;
  generation = 0;

// Main loop -- we have a population, we have a genetic code, and we
//   have data on which we can compute scores.
  do {
    #ifdef VERBOSE
      printf("in main loop, generation = %d\n",generation); fflush(stdout);
      fflush(stdout);
    #endif
    for (i = 0; i < POPULATION; i++) {
      #ifdef VERBOSE
        printf("in do loop, i = %d\n",i); fflush(stdout);
        fflush(stdout);
      #endif
      if (scores[i] == 0.0) {
        transcriber(genome[i], weights, x);
        scores[i] = scorer(sst, weights);
        #ifdef VERBOSE
          printf("trial %f ", scores[i]); fflush(stdout);
          showgenes(stdout, genome[i], x); fflush(stdout);
        #endif
      }

    }
// Now that we've gone through the whole population, check the 
//   scores
      best = scores.maximum();
      mean = scores.average();
      printf("generation %d, %d evaluations, scores: %f %f %f %f\n", generation,
         evaluations,
         scores.maximum(),  scores.average(),  scores.rms(),  scores.norm(4) );
      fflush(stdout);

// Now that we know how good the critters, are, put them somewhat into
//  score order (order) and then carry out reproduction:
      order(genome, scores);
      reproducer(genome, scores);

// For standard diagnostics, print out the top 15 list.  This is
//  very much discretionary.
      printf("\ngeneration %3d top 15 list\n", generation);
      for (i = 0; i < 15; i++) {
        printf("score %f ",scores[i]);
        showgenes(stdout, genome[i], x);
        fflush(stdout);
      }
      printf("\n");

// 'Graze' on the solutions -- ensure that there is not excessive similarity
//  between population members
      grazer(genome, scores);
      generation += 1;

    } while ( (best < 305.25) && (generation < genmax) );
//    } while ( (best > (1.+1./30.)*mean) && (generation < genmax) );
// The above is a stopping condition -- it says to continue until the
// mean is almost as good as the best.  For different problems, you might
// want this to be more like 1.+1/300, or 1 + 1/2, or the best to be
// greater than a cutoff value, or ... 
// Experiment, too, on what happens as you change the requirement for 
// best towards the data files absolute maximum


// Now that we're done, show what the final population looked like:
    printf("final best, mean %f %f ratio %f generation %d evals %d\n",
		    best, mean, best/mean, generation, evaluations);
    for (i = 0; i < genome.xpoints(); i++) {
      printf("%f :  ",scores[i]);
      showgenes(stdout, genome[i], x);
    }

  return 0;

}
float scorer(global_ice<float> &sst, mvector<united> &weights) {
  ijpt location;
  float score;

  evaluations += 1;
// Notice that in the scorer we _do_ have to have some knowledge of what
// the weights mean.
  location.i = weights[0].ival;
  location.j = weights[1].ival;
  
  score = sst[location];
// Other ideas:
//  score = sst[location]*sst[location];
// 4th power of the sst:
//  score = pow(sst[location],4.);
// 1/4th power of the sst:
//  score = pow(sst[location],1./4.);

  return score;
}
