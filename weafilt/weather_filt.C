#include <stdio.h>
#include "ncepgrids.h"

#include "icessmi.h"
#include "icegrids.h"

/* Perform extended weather filtering (per OMB Tech Note 120) on
     SSMI data for sea ice use */
/* Robert Grumbine 4 June 1997 */
 
float gr37(grid2<ssmipt> &map, const int i, const int j, const int range) ;
float gr22(grid2<ssmipt> &map, const int i, const int j, const int range) ;

/* Variables for tracking which filters get used */
/* 16 March 2004 */
extern int efilt_37_n, efilt_37_s;
extern int efilt_22_n, efilt_22_s;

int newfilt(northgrid<ssmipt> &nmap, southgrid<ssmipt> &smap)
{
  int i, j, index, indexp1, indexm1, indexjp1, indexjm1;
  float g37[NY_NORTH][NX_NORTH];
  float g22[NY_NORTH][NX_NORTH];
  int debug;
  northgrid<unsigned char> nconc;
  southgrid<unsigned char> sconc;
  int range;
  ijpt loc;
 
  #ifdef VERBOSE
    debug = (1 == 1);
  #else
    debug = (0 == 1);
  #endif
  #ifdef HIRES
    range = 1;
  #else
    range = 0;
  #endif

  getfld(nmap, nconc, &g37[0][0], SSMI_BAR_CONC);
  getfld(smap, sconc, &g37[0][0], SSMI_BAR_CONC);

/* Find the northern hemisphere gradient ratio */
  for (j = 0; j < nmap.ypoints()  ; j++) {
    for (i = 0; i < nmap.xpoints()  ; i++) {
      g37[j][i] = gr37(nmap, i, j, range);
      g22[j][i] = gr22(nmap, i, j, range);
    }
  }
/* Loop over all points.  If, in any case, the 2 pt averaged gradient ratio 
     for 37-19 v is greater than the cut off, then filter out the ice 
     concentration */

  for (loc.j = 1; loc.j < nmap.ypoints() - 1 ; loc.j++) {
    for (loc.i = 1; loc.i < nmap.xpoints() - 1 ; loc.i++) {
      if (nconc[loc] != 0 && nconc[loc] != BAD_DATA) {
        index = loc.i+loc.j*nmap.xpoints();
        indexp1 = index + 1; 
        indexm1 = index - 1; 
        indexjm1 = index - nmap.xpoints(); 
        indexjp1 = index - nmap.xpoints(); 
        /* Include this as either second check (low resolution) or
           an appropriately-ranged check (high resolution 
           Robert Grumbine 16 March 2004 */
        if (g37[loc.j][loc.i] > SSMI_GR37LIM) {
           efilt_37_n += 1;
           nconc[loc] = WEATHER;
        }
        if (g22[loc.j][loc.i] > SSMI_GR22LIM) {
           efilt_22_n += 1;
           nconc[loc] = WEATHER;
        }

        if (nconc[indexjp1] != BAD_DATA) {
          if (g37[loc.j+1][loc.i] + g37[loc.j][loc.i] > 2*SSMI_GR37LIM ) {
            efilt_37_n += 1;
            nconc[loc] = WEATHER;
            if (debug) printf("1 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (nconc[indexjm1] != BAD_DATA) {
          if (g37[loc.j-1][loc.i] + g37[loc.j][loc.i] > 2*SSMI_GR37LIM ) {
            efilt_37_n += 1;
            nconc[loc] = WEATHER;
            if (debug) printf("2 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (nconc[indexm1] != BAD_DATA) {
          if (g37[loc.j][loc.i-1] + g37[loc.j][loc.i] > 2*SSMI_GR37LIM ) {
            efilt_37_n += 1;
            nconc[loc] = WEATHER;
            if (debug) printf("3 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (nconc[indexp1] != BAD_DATA) {
          if (g37[loc.j][loc.i+1] + g37[loc.j][loc.i] > 2*SSMI_GR37LIM ) {
            efilt_37_n += 1;
            nconc[loc] = WEATHER;
            if (debug) printf("4 resetting %3d %3d \n",i,j);
            continue;
          }
        }
      
      } /* End of filtration testing */

    }
  }

  for (loc.j = 0; loc.j < nmap.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < nmap.xpoints(); loc.i++) {
       nmap[j*nmap.xpoints() +i].obs.bar_conc = nconc[loc];
    }
  }

/* Need to put southern filtering in here */
  for (j = 0; j < smap.ypoints()  ; j++) {
    for (i = 0; i < smap.xpoints()  ; i++) {
      g37[j][i] = gr37(smap, i, j, range);
      g22[j][i] = gr22(smap, i, j, range);
    }
  }
/* Note above that we've used the same array for both north and south
   gradients */
  for (loc.j = 1; loc.j < smap.ypoints() - 1; loc.j++) {
    for (loc.i = 1; loc.i < smap.xpoints() - 1; loc.i++) {
      if (sconc[loc] != 0 && sconc[loc] != BAD_DATA) {
        /* Include this as either second check (low resolution) or
           an appropriately-ranged check (high resolution 
           Robert Grumbine 16 March 2004 */
        if (g37[loc.j][loc.i] > SSMI_GR37LIM) {
           efilt_37_s += 1;
           sconc[loc] = WEATHER;
        }
        if (g22[loc.j][loc.i] > SSMI_GR22LIM) {
           efilt_22_s += 1;
           sconc[loc] = WEATHER;
        }

        if (sconc[indexjp1] != BAD_DATA) {
          if (g37[loc.j+1][loc.i] + g37[loc.j][loc.i] > 2*SSMI_GR37LIM ) {
            efilt_37_s += 1;
            sconc[loc] = WEATHER;
            if (debug) printf("1 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (sconc[indexjm1] != BAD_DATA) {
          if (g37[loc.j-1][loc.i] + g37[loc.j][loc.i] > 2*SSMI_GR37LIM ) {
            efilt_37_s += 1;
            sconc[loc] = WEATHER;
            if (debug) printf("2 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (sconc[indexm1] != BAD_DATA) {
          if (g37[loc.j][loc.i-1] + g37[loc.j][loc.i] > 2*SSMI_GR37LIM ) {
            efilt_37_s += 1;
            sconc[loc] = WEATHER;
            if (debug) printf("3 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (sconc[indexp1] != BAD_DATA) {
          if (g37[loc.j][loc.i+1] + g37[loc.j][loc.i] > 2*SSMI_GR37LIM ) {
            efilt_37_s += 1;
            sconc[loc] = WEATHER;
            if (debug) printf("4 resetting %3d %3d \n",i,j);
            continue;
          }
        }
      
      } /* End of filtration testing */

    }
  }

  for (loc.j = 0; loc.j < smap.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < smap.xpoints(); loc.i++) {
       smap[j*smap.xpoints() + i].obs.bar_conc = sconc[loc];
    }
  }

  return 0;
}


float gr37(grid2<ssmipt> &map, const int i, const int j, const int range) {
   int index, ti, tj, count = 0;
   float t19v = 0.0, t37v = 0.0;

   if (range != 0) {
     for (tj = j-range ; tj < j+range ; tj++) {
       for (ti = i - range ; ti < i+range; ti++) {
         index = ti + tj*map.xpoints();
         if (index < 0 || index >= map.xpoints()*map.ypoints()) continue;
         if (map[index].obs.t19v != 0 && map[index].obs.t37v != 0) {
           count += 1;
           t19v += map[index].obs.t19v;
           t37v += map[index].obs.t37v;
         }
       }
     }

     t37v = t37v / count;
     t19v = t19v / count;
   }
   else {
     index = i + j*map.xpoints();
     t19v = map[index].obs.t19v;
     t37v = map[index].obs.t37v;
   }

   if (t19v != 0.0 && t37v != 0.0 ) {
     return ((t37v - t19v) / (t37v + t19v));
   }
   else {return 0.0;}

   return 0.0;
}

float gr22(grid2<ssmipt> &map, const int i, const int j, const int range) {
   int index, ti, tj, count = 0;
   float t19v = 0.0, t22v = 0.0;

   if (range != 0) {
     for (tj = j-range ; tj < j+range ; tj++) {
       for (ti = i - range ; ti < i+range; ti++) {
         index = ti + tj*map.xpoints();
         if (index < 0 || index >= map.xpoints()*map.ypoints()) continue; 
         if (map[index].obs.t19v != 0 && map[index].obs.t22v != 0) {
           count += 1;
           t19v += map[index].obs.t19v;
           t22v += map[index].obs.t22v;
         }
       }
     }
  
     t22v = t22v / count;
     t19v = t19v / count;
   }
   else {
     index = i + j*map.xpoints();
     t19v = map[index].obs.t19v;
     t22v = map[index].obs.t22v;
   }

   if (t19v != 0.0 && t22v != 0.0 ) {
     return ((t22v - t19v) / (t22v + t19v));
   }
   else {return 0.0;}
   
   return 0.0;
}
   
