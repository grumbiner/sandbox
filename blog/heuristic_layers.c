#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* Illustrate Robert Grumbine blog post 
    http://moregrumbinescience.blogspot.com/2010/02/heuristic-stratospheric-cooling.html */
/* Execute with the argument being the number of layers you want.  Must be between 1 and MAXLAYERS-1. */

/* Note: Program run time is proportional to nlayers^4  3.4 seconds for 100 layers on my machine,
          53 seconds for 200 layers, and would be about 15 minutes for 400 layers.  1000 layers would
          be a good long time */

/* Robert Grumbine 2 February 2010 */

#define MAXLAYERS 1025

void photon_tracker(double *layers, double *space, double *ground, int nl);

int main(int argc, char *argv[]) {
  double layers[MAXLAYERS], space, ground;
  int top;
  int i, j;

  top = atoi(argv[1]);
  printf("Working with %d layers\n",top);

  for (i = 1; i <= top; i++) {
    space = 0.0; 
    ground = 0.0;
    for (j = 0; j <= top; j++) {
      layers[i] = 0.0;
    }
    layers[i] = 1024.0;
    photon_tracker(layers, &space, &ground, top);
    printf("starting from layer %4d space %8.3f ground %8.3f net warming of %9.3f\n",i, space, ground, ground-space);
  }

  return 0;
}

void photon_tracker(double *layers, double *space, double *ground, int nl) {
  double tempor[MAXLAYERS];
  float photons, initial;
  int i, iteration = 0;

  *space = 0;
  *ground = 0;
  photons = 0.0;
/* Initialize the temporary array to zero and count up how many photons are still in the atmosphere */
  for (i = 1; i <= nl; i++) {
    tempor[i] = 0;
    photons += layers[i];
  }
  initial = photons;


/* While there are still photons in the atmosphere, slosh them around */
  while (photons > 1e-10) {

     /* Main loop -- send photons from layer i to the temporary vector in layers i+1 and i-1 */
     /* Note that layer 0 = ground, layer nl + 1 = space */
     for (i = 1; i <= nl; i++) {
       tempor[i-1] += 0.5*layers[i]; 
       tempor[i+1] += 0.5*layers[i]; 
     } 

     /* Now update the atmosphere and our photon counter */
     *ground += tempor[0];
     *space  += tempor[nl+1]; 
     tempor[0] = 0;
     tempor[nl+1] = 0;

     photons = 0.0;
     for (i = 0; i <= nl+1; i++) {
       layers[i] = tempor[i];
       photons += layers[i];
       tempor[i] = 0;
     } 

     iteration += 1;
     /* printf("iteration %d, atm photon count %e ground %e space %e total %e\n",
                  iteration, photons, ground, space, photons+ground+space); */
     if (fabs(initial - photons - *ground - *space) > 1) {
       printf("have lost photons somewhere\n");
       printf("Ground = %e, space = %e\n",*ground, *space);
       for (i = 0; i <= nl+1; i++) {
          printf("%d  layers %e tempor %e\n",i,layers[i], tempor[i]);
       }
       exit(1);
     }
  }
  printf("%d iterations ",iteration);

  return;
}
