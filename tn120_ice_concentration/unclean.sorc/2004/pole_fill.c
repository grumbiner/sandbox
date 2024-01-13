#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "ssmi.h"
#include "icessmi.h"

#include "icegrids.h"

/* Routine to fill in the unobserved polar gap */
/* Use a laplacean fill on the principle that the true field
   there is the smoothest (gradient sense) possible */
/* Robert Grumbine 4 June 1997 */

float max(float x, float y);
float max(float x, float y) {
  if (x > y) {
    return x;
  }
  return y;
}

int pole_fill(unsigned char *map, const int pole)
{
    int i, j;
    float *a0, *a1, *a2, *delta;
    int nx, ny, polei, polej;
    int index, indexori, holex, holey;
    int iters, itmax = 15, jp, jm, ip, im;
    float delmax;

    if (dx != dy) {
      printf("Cannot use this polefill routine, it assumes that dx = dy.\n");
      printf(" dx = %f dy = %f\n",dx, dy);
      return -1;
    }

    if (pole == 1) {
      nx = NX_NORTH;
      ny = NY_NORTH;
      polei = (int) (0.5 + polei_NORTH);
      polej = (int) (0.5 + polej_NORTH);
    }
    else {
      nx = NX_SOUTH;
      ny = NY_SOUTH;
      polei = (int) (0.5 + polei_SOUTH);
      polej = (int) (0.5 + polej_SOUTH);
    }

    holex = (int) (0.5 + 2.*( 0.5 + (90. - MAX_LATITUDE) * 111.e3 / dx ) + 3.);
    holey = holex;

    printf("holex, holey = %d %d \n",holex, holey);

    a0 = (float *) malloc(sizeof(float)*holex*holey);
    a1 = (float *) malloc(sizeof(float)*holex*holey);
    a2 = (float *) malloc(sizeof(float)*holex*holey);
    delta = (float *) malloc(sizeof(float)*holex*holey);

/* Make copy for local use */
    for (j = 0; j < holey; j++)
    { for (i = 0; i < holex; i++)
      {
        index = i+j*holex;
        indexori = (polei - holex/2 + i) + (polej - holey/2 + j)*nx;
        a0[index] = (float) map[indexori];
      }
    } 

/* Fill in the 'no data' or 'bad data' points */
   for (j = 0; j < holey  ; j++)
   { for (i = 0; i < holex  ; i++)
     {
       index = i + j*holex;
       jp =  i      + (holey-1)*holex;
       jm =  i      +   0*holex;
       ip = holex-1 + j*holex;
       im =  0      + j*holex;
       if (a0[index] == NO_DATA || a0[index] == BAD_DATA || a0[index] == 0) {
         if ( a0[im] == NO_DATA || a0[ip] == NO_DATA || 
              a0[im] == BAD_DATA || a0[ip] == BAD_DATA || 
              a0[jm] == NO_DATA || a0[jp] == NO_DATA || 
              a0[jm] == BAD_DATA || a0[jp] == BAD_DATA ) { 
           printf("Data problem with pole filling, no or bad data on bndy\n");
           return -1;
         } 
         a1[index] = 0.5 *(a0[im] + (float)(i)/(float)(holex) * 
                                     (a0[ip] - a0[im]) )
                   + 0.5 *(a0[jm] + (float)(j)/(float)(holey) * 
                                     (a0[jp] - a0[jm]) );
       }
       else {
         a1[index] = a0[index];
       }
     }
   }

/* Now start some sort of an iteration scheme on the interior values */
   iters = 0;
   do {
     delmax = 0.;
     iters += 1;

     for (j = 1; j < holey-1; j++)
     { for ( i = 1; i < holex-1; i++)
       {
         index = i + j*holex;
         im =  i-1 + j*holex;
         ip =  i+1 + j*holex;
         jm =  i + (j-1)*holex;
         jp =  i + (j+1)*holex;
        
         delta[index] = -a1[index] + (a1[im] + a1[ip] + a1[jm] + a1[jp])/4.;
         a2[index] = a1[index]+delta[index];

         delmax = max(fabs(delta[index]), delmax);
       }
     }
     printf("delmax = %f\n",delmax);
     for (j = 1; j < holey-1; j++)
     { for ( i = 1; i < holex-1; i++)
     {  index = i + j*holex;
        a1[index] = a2[index];
     }
     }

   }
   while ( fabs(delmax) > 0.5 && iters < itmax );

    for (j = 0; j < holey; j++)
    { for (i = 0; i < holex; i++)
      {
        index = i+j*holex;
        indexori = (polei - holex/2 + i) + (polej - holey/2 + j)*nx;
        if ( a0[index] == BAD_DATA || a0[index] == NO_DATA || a0[index] == 0) {
          map[indexori] = (int) (0.5 + a1[index]) ;
        }
      }
    } 

   free(a0);
   free(a1);
   free(a2);
   free(delta);

   return 0;

}
