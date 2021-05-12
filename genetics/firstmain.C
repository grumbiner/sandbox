#include <iostream>
#include <math.h>
#include "ncepgrids.h"
#include "mvector.h"
#include "program.h"
#include "cell.h"

// Main program for genetic algorithms

// Interpreter is a simple function operating from a mvector input (program),
//  on some data (input mvector) and producing output.

#define NPROGS 1000

#include "ssmipt.h"
extern void interpreter(program &pgm, mvector<float> &inputs, mvector<float> &outputs);
extern void selector1(mvector<program> &population, float &best) ;
extern void getin(grid2<ssmipt> &x);
#include "interp.C"
#include "all.selectors.C"
#include "subs.C"

int main(int argc, char *argv[]) {
  mvector<program> population(NPROGS);

  mvector<mvector<float> > inputs(NPROGS);
  mvector<float> true_outputs(NPROGS);

  mvector<float> outputs(NPROGS);
  mvector<float> fullvec(9), invec(7), outvec(1);

  northgrid<unsigned char> mask;
  grid2<ssmipt> fullin(mask.xpoints(), mask.ypoints() );

  int i, j, generation;
  float toler = 0.01, sum, best;

// Start up the random number generator
   if (argc > 1) {
     srand(atoi(argv[1]) );
   }
   else {
     srand(0);
   }

// Set up initial sizes for the programs
   for (i = 0; i < NPROGS; i++) {
     population[i].resize(24);
     inputs[i].resize(7);   // 7 Tb's
   }

// Initialize the first program (seed the population)

// Read in a data file for inputs and outputs
  getin(fullin);
  return 1;

// Mutate the 0th program through the rest of the domain

  best = FLT_MAX;
  generation = 0;
  while (best > toler * FLT_MAX) {
    generation += 1;
    sum = 0.;
    for (i = 0; i < population.xpoints() ; i++) {  // for each program
      for (j = 0; j < true_outputs.xpoints(); j++) {     // for each in/out pair
        invec = inputs[j];
        interpreter(population[i], invec, outvec); 
        sum += (outvec[0] - true_outputs[j])*(outvec[0] - true_outputs[j]);
      }
      population[i].value = sum;
      best = min(best, sum);
    }

    selector1(population, best); // Selector is also doing the mutation and
                                //  reproduction.
    cout << "generation " << generation << "best score is " << best << endl;

  } //end while

   return 0;
}
