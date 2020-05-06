#include <stdio.h>
#include <math.h>
#include "icessmi.h"
#include "icegrids.h"

/* Perform extended weather filtering (per OMB Tech Note 120) on
     SSMI data for sea ice use */
/* Robert Grumbine 4 June 1997 */
 
float gr37(const ssmi *map, const int i, const int j, 
           const int nx, const int ny, const int range);
float gr22(const ssmi *map, const int i, const int j, 
           const int nx, const int ny, const int range);


int newfilt(ssmi *nmap, ssmi *smap)
{
  int i, j;
  float t19v, t19h, t22v, t37v, t37h, t85v, t85h;
  float g37[NY_NORTH][NX_NORTH];
  int debug;
  unsigned char nconc[NY_NORTH][NX_NORTH], sconc[NY_SOUTH][NX_SOUTH];
  
  debug = (0 == 1);

  getfld(nmap, NX_NORTH*NY_NORTH, &nconc[0][0], &g37[0][0], BAR_CONC);
  getfld(smap, NX_SOUTH*NY_SOUTH, &sconc[0][0], &g37[0][0], BAR_CONC);

/* Find the northern hemisphere gradient ratio */
  for (j = 0; j < NY_NORTH  ; j++) {
    for (i = 0; i < NX_NORTH  ; i++) {
      g37[j][i] = gr37(nmap, i, j, NX_NORTH, NY_NORTH, 0);
    }
  }
/* Loop over all points.  If, in any case, the 2 pt averaged gradient ratio 
     for 37-19 v is greater than the cut off, then filter out the ice 
     concentration */

  for (j = 1; j < NY_NORTH - 1 ; j++) {
    for (i = 1; i < NX_NORTH - 1 ; i++) {
      if (nconc[j][i] != 0 && nconc[j][i] != BAD_DATA) {

        if (nconc[j+1][i] != BAD_DATA) {
          if (g37[j+1][i] + g37[j][i] > 2*GR37LIM ) {
            nconc[j][i] = (float) WEATHER;
            if (debug) printf("1 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (nconc[j-1][i] != BAD_DATA) {
          if (g37[j-1][i] + g37[j][i] > 2*GR37LIM ) {
            nconc[j][i] = (float) WEATHER;
            if (debug) printf("2 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (nconc[j][i-1] != BAD_DATA) {
          if (g37[j][i-1] + g37[j][i] > 2*GR37LIM ) {
            nconc[j][i] = (float) WEATHER;
            if (debug) printf("3 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (nconc[j][i+1] != BAD_DATA) {
          if (g37[j][i+1] + g37[j][i] > 2*GR37LIM ) {
            nconc[j][i] = (float) WEATHER;
            if (debug) printf("4 resetting %3d %3d \n",i,j);
            continue;
          }
        }
      
      } /* End of filtration testing */

    }
  }

  for (j = 0; j < NY_NORTH; j++) {
    for (i = 0; i < NX_NORTH; i++) {
       nmap[j*NX_NORTH +i].bar_conc = nconc[j][i];
    }
  }

/* Need to put southern filtering in here */
  for (j = 0; j < NY_SOUTH  ; j++) {
    for (i = 0; i < NX_SOUTH  ; i++) {
      g37[j][i] = gr37(smap, i, j, NX_SOUTH, NY_SOUTH, 0);
    }
  }
/* Note above that we've used the same array for both north and south
   gradients */
  for (j = 1; j < NY_SOUTH - 1; j++) {
    for (i = 1; i < NX_SOUTH - 1; i++) {
      if (sconc[j][i] != 0 && sconc[j][i] != BAD_DATA) {

        if (sconc[j+1][i] != BAD_DATA) {
          if (g37[j+1][i] + g37[j][i] > 2*GR37LIM ) {
            sconc[j][i] = (float) WEATHER;
            if (debug) printf("1 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (sconc[j-1][i] != BAD_DATA) {
          if (g37[j-1][i] + g37[j][i] > 2*GR37LIM ) {
            sconc[j][i] = (float) WEATHER;
            if (debug) printf("2 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (sconc[j][i-1] != BAD_DATA) {
          if (g37[j][i-1] + g37[j][i] > 2*GR37LIM ) {
            sconc[j][i] = (float) WEATHER;
            if (debug) printf("3 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (sconc[j][i+1] != BAD_DATA) {
          if (g37[j][i+1] + g37[j][i] > 2*GR37LIM ) {
            sconc[j][i] = (float) WEATHER;
            if (debug) printf("4 resetting %3d %3d \n",i,j);
            continue;
          }
        }
      
      } /* End of filtration testing */

    }
  }

  for (j = 0; j < NY_SOUTH; j++) {
    for (i = 0; i < NX_SOUTH; i++) {
       smap[j*NX_SOUTH + i].bar_conc = sconc[j][i];
    }
  }

  return 0;
}

float gr37(const ssmi *map, const int i, const int j, 
                            const int nx, const int ny, const int range)
{
   int index, ti, tj, count;
   float t19v, t37v, tempor;

   t19v = 0.0;
   t37v = 0.0;
   count = 0;
 
   if (range != 0) {
     for (tj = j-range ; tj < j+range ; tj++) {
       for (ti = i - range ; ti < i+range; ti++) {
         index = ti + tj*nx;
         if (index < 0 || index >= nx*ny) continue; 
         if (map[index].t19v != 0 && map[index].t37v != 0) {
           count += 1;
           t19v += map[index].t19v;
           t37v += map[index].t37v;
         }
       }
     }
  
     t37v = t37v / count;
     t19v = t19v / count;
   }
   else {
     index = i + j*nx;
     t19v = map[index].t19v;
     t37v = map[index].t37v;
   }

   if (t19v != 0.0 && t37v != 0.0 ) {
     tempor = (t37v - t19v) / (t37v + t19v);
   }
   else {tempor = 0.0;}
   
   return tempor;
}


float gr22(const ssmi *map, const int i, const int j, 
                            const int nx, const int ny, const int range)
{
   float t19v, t22v, tempor;
   int index, ti, tj, count;

   t19v = 0.0;
   t22v = 0.0;
   count = 0;
 
   for (tj = j-range ; tj < j+range ; tj++) {
     for (ti = i - range ; ti < i+range; ti++) {
        count += 1;
        index = i + j*nx;
        if (index < 0 || index >= nx*ny) continue; 
        t19v += map[index].t19v;
        t22v += map[index].t22v;
     }
   }
   t19v = t19v / count;
   t22v = t22v / count;

   tempor = (t22v - t19v) / (t22v + t19v);
   
   return tempor;
}
