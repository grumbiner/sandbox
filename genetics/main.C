#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <iostream>
#include "ncepgrids.h"
#include "mvector.h"

// Main program for genetic algorithms

// Interpreter is a simple function operating from a mvector input (program),
//  on some data (input mvector) and producing output.

#define NPROGS 1000
#define GEN_MAX 5
#define MAXREGISTERS 21
#define NDATA 179025


#include "ssmipt.h"
#include "program.h"

#include "interp2.C"
#include "all.selectors.C"
#include "subs.C"

void interpreter(program &pgm, mvector<float> &inputs, mvector<float> &outputs);
void selector3(program *population, float &best) ;
void getin(grid2<ssmipt> &x);

int main(int argc, char *argv[]) {
  //mvector<program> population(NPROGS);
  program population[NPROGS];

  northgrid<unsigned char> mask;
  grid2<ssmipt> fullin(mask.xpoints(), mask.ypoints() );

  mvector<mvector<float> > inputs(NDATA);
  mvector<float> outputs(NDATA);
  mvector<float> fullvec(9), invec(7), outvec(1);

  int i, j, generation, icepts;
  float toler = 5.e9, sum, best;
  ijpt loc;

// Read in a data file for inputs and outputs, then loop over grid
//   and pull out nominal sea ice points, transferring to 
  getin(fullin);
  icepts = 0;
  for (loc.j = 0; loc.j < fullin.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < fullin.xpoints(); loc.i++) {
    inputs[icepts].resize(7);   // 7 Tb's
    fullin[loc].tovec(fullvec);
    if (fullvec[8] < 1.28) {
      outputs[icepts] = fullvec[8];
      for (j = 0; j < 7; j++) {
        inputs[icepts][j] = fullvec[j];
      }
      icepts += 1;
    }
  }
  }
  icepts -= 1;
  icepts /= 100;
  printf("Found %d ice points for the algorithm\n", icepts); fflush(stdout);


// Start up the random number generator
   if (argc > 1) {
     srand(atoi(argv[1]) );
   }
   else {
     srand(0);
   }

////////Initialize the programs
// Set up initial sizes for the programs
   for (i = 0; i < NPROGS; i++) {
     population[i].resize(48);
   }

// Initialize the first program (seed the population)
   for (i = 0; i < population[0].length(); i++) {
      population[0][i] = 4.*rand()/(RAND_MAX + 1.0);
   }

// Mutate the 0th program through the rest of the domain
   for (i = 1; i < NPROGS; i++) {
      population[0].mutate(population[i]) ;
   }
////////////////////////////////////


// Finally, begin the loop over generations until we get a fit 
  best = FLT_MAX;
  generation = 0;
  while (best > toler && generation < GEN_MAX) {
    generation += 1;

    for (i = 0; i < NPROGS ; i++) {  // for each program
      sum = 0.;
      for (j = 0; j < icepts; j++) {     // for each in/out pair
        invec = inputs[j];
        interpreter(population[i], invec, outvec); 
        sum += (outvec[0] - outputs[j])*(outvec[0] - outputs[j]);
      }
      population[i].value = sum;
      cout << "Score of program " << i << " is "<< sum << " generation "; 
      cout << generation << endl;
      best = min(best, sum);
    }
    

    printf("About to call on the selector\n"); fflush(stdout);
    selector3(population, best); // Selector is also doing the mutation and
                                //  reproduction.

  } //end while
   printf("end best = %f\n",best ); fflush(stdout);

   return 0;
}
