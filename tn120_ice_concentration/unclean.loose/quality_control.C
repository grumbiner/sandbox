#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "ncepgrids.h"

#include "ssmiclass.h"
#include "icessmi.h"
#include "icegrids.h"

#define ABDALATI 1
int getfld(grid2<ssmipt> &ice, grid2<unsigned char> &fld, float *rfld, int sel);
int newfilt(northgrid<ssmipt> &nmap, southgrid<ssmipt> &smap);

extern "C" int pole_fill(unsigned char *map, int x);
extern "C" float nasa_team(float t19v, float f19h, float t22v, float t37v, float t37h,
                float t85v, float t85h, const char pole, const int ant,
                const int regress);


int main(int argc, char *argv[]) {
  FILE *nin, *nout, *sin, *sout;
  FILE *newnin, *newsin;
  northgrid<ssmipt> nmap, nmapnew;
  southgrid<ssmipt> smap, smapnew;
  northgrid<unsigned char> nconc, nconcnew, orig_nconc, norig_new, abar_n, bar_a_n;
  southgrid<unsigned char> sconc, sconcnew, orig_sconc, sorig_new, abar_s, bar_a_s;

  float t19v, t19h, t22v, t37v, t37h, t85v, t85h;
  float *g37;
  float toler = 1;
  ijpt loc;
  int i;
  latpt ll;

  g37 = (float *) malloc(nmap.xpoints() * nmap.ypoints() *sizeof(float) );

  nin = fopen(argv[1], "r");
  sin = fopen(argv[2], "r");
  newnin = fopen(argv[3], "r");
  newsin = fopen(argv[4], "r");
  if (nin == (FILE *) NULL || sin == (FILE *) NULL || 
      newnin == (FILE *) NULL || newsin == (FILE *) NULL ) {
    printf("Failed to open an input file\n");
    return -1;
  }

  i = nmap.binin(nin);
  if (i != nmap.xpoints()*nmap.ypoints() ) {
    printf("failed to read in full map! %d of %d\n",i, nmap.xpoints()*nmap.ypoints());
    return -1;
  }
  i = smap.binin(sin);
  if (i != smap.xpoints()*smap.ypoints() ) {
    printf("failed to read in full map! %d of %d\n",i, smap.xpoints()*smap.ypoints());
    return -1;
  }
  i = nmapnew.binin(newnin);
  if (i != nmapnew.xpoints()*nmapnew.ypoints() ) {
    printf("failed to read in full map! %d of %d\n",i, 
             nmapnew.xpoints()*nmapnew.ypoints());
    return -1;
  }
  i = smapnew.binin(newsin);
  if (i != smapnew.xpoints()*smapnew.ypoints() ) {
    printf("failed to read in full map! %d of %d\n",i, 
              smapnew.xpoints()*smapnew.ypoints());
    return -1;
  }

  getfld(nmap, orig_nconc, g37, CONC_BAR);
  getfld(smap, orig_sconc, g37, CONC_BAR);
  getfld(nmapnew, norig_new, g37, CONC_BAR);
  getfld(smapnew, sorig_new, g37, CONC_BAR);
// -------------- Part 2 -- compare a(bar) vs bar(a) ------------------------
  abar_n = orig_nconc;
  abar_s = orig_sconc;
  getfld(nmap, bar_a_n, g37, BAR_CONC);
  getfld(smap, bar_a_s, g37, BAR_CONC);
  for (loc.j = 0; loc.j < abar_n.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < abar_n.xpoints(); loc.i++) {
    if (abar_n[loc] != bar_a_n[loc] && bar_a_n[loc] != WEATHER && bar_a_n[loc] != BAD_DATA  ) {
      printf("orig bara vs abar north %3d %3d  %3d %3d  %4d\n",loc.i, loc.j, 
               bar_a_n[loc], abar_n[loc], bar_a_n[loc] - abar_n[loc] ); 
    }
  }
  }
  for (loc.j = 0; loc.j < abar_s.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < abar_s.xpoints(); loc.i++) {
    if (abar_s[loc] != bar_a_s[loc] && bar_a_s[loc] != WEATHER && bar_a_s[loc] != BAD_DATA ) {
      printf("orig bara vs abar south %3d %3d  %3d %3d  %4d\n",loc.i, loc.j, 
               bar_a_s[loc], abar_s[loc], bar_a_s[loc] - abar_s[loc] ); 
    }
  }
  }

  abar_n = norig_new;
  abar_s = sorig_new;
  getfld(nmapnew, bar_a_n, g37, BAR_CONC);
  getfld(smapnew, bar_a_s, g37, BAR_CONC);
  for (loc.j = 0; loc.j < abar_n.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < abar_n.xpoints(); loc.i++) {
    if (abar_n[loc] != bar_a_n[loc] && bar_a_n[loc] != WEATHER && bar_a_n[loc] != BAD_DATA ) {
      printf("new  bara vs abar north %3d %3d  %3d %3d  %4d\n",loc.i, loc.j, 
               bar_a_n[loc], abar_n[loc], bar_a_n[loc] - abar_n[loc] ); 
    }
  }
  }
  for (loc.j = 0; loc.j < abar_s.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < abar_s.xpoints(); loc.i++) {
    if (abar_s[loc] != bar_a_s[loc] && bar_a_s[loc] != WEATHER && bar_a_s[loc] != BAD_DATA ) {
      printf("new  bara vs abar south %3d %3d  %3d %3d  %4d\n",loc.i, loc.j, 
               bar_a_s[loc], abar_s[loc], bar_a_s[loc] - abar_s[loc] ); 
    }
  }
  }
 


// -------------- Part 4 -- compare new vs old ------------------------------
  for (loc.j = 0; loc.j < nmap.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < nmap.xpoints(); loc.i++) {
      if (orig_nconc[loc] != norig_new[loc] && 
          (orig_nconc[loc] <= MAX_CONC && norig_new[loc] <= MAX_CONC) &&
          (orig_nconc[loc] >= MIN_CONC && norig_new[loc] >= MIN_CONC) ) {
        ll = nmap.locate(loc);
        printf("%3d %3d  %6.2f %7.2f  %3d %3d  %4d   %2d %2d %3d\n",
                loc.i, loc.j, 
                ll.lat, ll.lon,
                orig_nconc[loc], norig_new[loc],
                norig_new[loc] - orig_nconc[loc],
                nmap[loc].obs.count, nmapnew[loc].obs.count,
                nmap[loc].obs.count - nmapnew[loc].obs.count);
      }
    }
  }
  for (loc.j = 0; loc.j < smap.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < smap.xpoints(); loc.i++) {
      if (orig_sconc[loc] != sorig_new[loc] &&
          (orig_sconc[loc] <= MAX_CONC && sorig_new[loc] <= MAX_CONC) && 
          (orig_sconc[loc] >= MIN_CONC && sorig_new[loc] >= MIN_CONC) ) {
        ll = smap.locate(loc);
        printf("%3d %3d  %6.2f %7.2f  %3d %3d  %4d   %2d %2d %3d\n",
                loc.i, loc.j, 
                ll.lat, ll.lon,
                orig_sconc[loc], sorig_new[loc],
                sorig_new[loc] - orig_sconc[loc],
                smap[loc].obs.count, smapnew[loc].obs.count, 
                smap[loc].obs.count - smapnew[loc].obs.count);
      }
    }
  }

// -------------- Part 3 -- recompute the grids -----------------------------
  for (loc.j = 0; loc.j < nmap.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < nmap.xpoints(); loc.i++) {
       t19v = nmapnew[loc].obs.t19v / 100.;
       t19h = nmapnew[loc].obs.t19h / 100.;
       t22v = nmapnew[loc].obs.t22v / 100.;
       t37v = nmapnew[loc].obs.t37v / 100.;
       t37h = nmapnew[loc].obs.t37h / 100.;
       t85v = nmapnew[loc].obs.t85v / 100.;
       t85h = nmapnew[loc].obs.t85h / 100.;
       nmapnew[loc].obs.conc_bar = (unsigned char) (0.5 + 
            nasa_team(t19v, t19h, t22v, t37v, t37h, t85v, t85h, 'n', 1, ABDALATI) );
       if (nmapnew[loc].obs.conc_bar < (unsigned char) MIN_CONC) {
         nmapnew[loc].obs.conc_bar = 0;
       }

    }
  }

  for (loc.j = 0; loc.j < smapnew.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < smapnew.xpoints(); loc.i++) {
       t19v = smapnew[loc].obs.t19v / 100.;
       t19h = smapnew[loc].obs.t19h / 100.;
       t22v = smapnew[loc].obs.t22v / 100.;
       t37v = smapnew[loc].obs.t37v / 100.;
       t37h = smapnew[loc].obs.t37h / 100.;
       t85v = smapnew[loc].obs.t85v / 100.;
       t85h = smapnew[loc].obs.t85h / 100.;
       smapnew[loc].obs.conc_bar = (unsigned char) (0.5 + 
            nasa_team(t19v, t19h, t22v, t37v, t37h, t85v, t85h, 's', 1, ABDALATI) );
       if (smapnew[loc].obs.conc_bar < (unsigned char) MIN_CONC) {
         smapnew[loc].obs.conc_bar = 0;
       }
    }
  }

  newfilt(nmapnew, smapnew);

/* Fill in the polar gap */
  getfld(nmapnew, nconc, g37, CONC_BAR);
  getfld(smapnew, sconc, g37, CONC_BAR);

  pole_fill(nconc.grid, 1);
  pole_fill(sconc.grid, 2);

/* Look around at whether things have changed much, and where: */
  for (loc.j = 0; loc.j < nconc.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < nconc.xpoints(); loc.i++) {
      if (fabs((float)(nconc[loc] - norig_new[loc]) ) >= toler 
           && nconc[loc] != WEATHER
           && nconc[loc] != NO_DATA
           && nconc[loc] != BAD_DATA 
           && nconc[loc] != LAND
           && nconc[loc] != COAST
           && norig_new[loc] != WEATHER
           && norig_new[loc] != NO_DATA
           && norig_new[loc] != BAD_DATA 
           && norig_new[loc] != LAND      
           && norig_new[loc] != COAST      
           && norig_new[loc] >= MIN_CONC      
      ) {
        printf("large delta n %3d %3d  %3d %3d  %4d\n",loc.i,loc.j,norig_new[loc],
                    nconc[loc], nconc[loc] - norig_new[loc]);
      }
    }
  }

  for (loc.j = 0; loc.j < sconc.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < sconc.xpoints(); loc.i++) {
      if (fabs((float) sconc[loc] - sorig_new[loc] ) >= toler  
           && sconc[loc] != WEATHER
           && sconc[loc] != NO_DATA
           && sconc[loc] != BAD_DATA 
           && sconc[loc] != LAND
           && sconc[loc] != COAST
           && sorig_new[loc] != WEATHER
           && sorig_new[loc] != NO_DATA
           && sorig_new[loc] != BAD_DATA 
           && sorig_new[loc] != LAND      
           && sorig_new[loc] != COAST      
           && sorig_new[loc] >= MIN_CONC      
      ) {
        printf("large delta s %3d %3d  %3d %3d  %4d\n",loc.i,loc.j,sorig_new[loc],
                    sconc[loc], sconc[loc] - sorig_new[loc]);
      }
    }
  }

  
  return 0;
}

/* For the algorithm */
int bad_low = 0;
int bad_high = 0;
int crop_low = 0;
int filt37   = 0;
int filt22   = 0;
/* For filtration */
int efilt_37_n = 0;
int efilt_37_s = 0;
int efilt_22_n = 0;
int efilt_22_s = 0;

// C++ version 7 April 2004

int getfld(grid2<ssmipt> &ice, grid2<unsigned char> &cfld, float *ffld, int sel) {
/* Extract a desired field (specified by sel) from a full ssmi map
   (ice) and copy to both a character array (cfld) and a floating
   point array (ffdl) for some other routine to use.
   Tb Floats are scaled into degrees Kelvin, and have a 0.01 degree precision.
   Tb Chars are linearly rescaled according to o = (i-50)/2, where i is 
     the floating number input, and o is the output, with 2 degree precision
     starting from 50 Kelvin.
   Ice concentrations are 1% precision, floating or character.
   Robert Grumbine 11 October 1994.
*/

  int i, npts;

  npts = ice.xpoints() * ice.ypoints();
  if (cfld.xpoints() != ice.xpoints() || 
      cfld.ypoints() != ice.ypoints() ) {
    printf("grid mismatch %d %d vs. %d %d\n", cfld.xpoints(), cfld.ypoints(),
      ice.xpoints(), ice.ypoints() );
    fflush(stdout);
  }

  switch (sel) {
  case T19V :
    for (i = 0; i < npts; i++)
    {
      ffld[i] = (float) ice[i].obs.t19v/100.;
      cfld[i] = (unsigned char) (0.5 + (ffld[i] - 50.) / 2. );
    }
    break;
  case T19H :
    for (i = 0; i < npts; i++)
    {
      ffld[i] = (float) ice[i].obs.t19h/100.;
      cfld[i] = (unsigned char) (0.5 + (ffld[i] - 50.) / 2. );
    }
    break;
  case T22V :
    for (i = 0; i < npts; i++)
    {
      ffld[i] = (float) ice[i].obs.t22v/100.;
      cfld[i] = (unsigned char) (0.5 + (ffld[i] - 50.) / 2. );
    }
    break;
  case T37V :
    for (i = 0; i < npts; i++)
    {
      ffld[i] = (float) ice[i].obs.t37v/100.;
      cfld[i] = (unsigned char) (0.5 + (ffld[i] - 50.) / 2. );
    }
    break;
  case T37H :
    for (i = 0; i < npts; i++)
    {
      ffld[i] = (float) ice[i].obs.t37h/100.;
      cfld[i] = (unsigned char) (0.5 + (ffld[i] - 50.) / 2. );
    }
    break;
  case T85V :
    for (i = 0; i < npts; i++)
    {
      ffld[i] = (float) ice[i].obs.t85v/100.;
      cfld[i] = (unsigned char) (0.5 + (ffld[i] - 50.) / 2. );
    }
    break;
  case T85H :
    for (i = 0; i < npts; i++)
    {
      ffld[i] = (float) ice[i].obs.t85h/100.;
      cfld[i] = (unsigned char) (0.5 + (ffld[i] - 50.) / 2. );
    }
    break;
  case CONC_BAR :
    for (i = 0; i < npts; i++) {
      ffld[i] = (float) ice[i].obs.conc_bar/100.;
      cfld[i] = (unsigned char) (0.5 + ice[i].obs.conc_bar) ;
    }
    break;
  case BAR_CONC :
    for (i = 0; i < npts; i++) {
      ffld[i] = (float) ice[i].obs.bar_conc/100.;
      cfld[i] = (unsigned char) ice[i].obs.bar_conc ;
    }
    break;
  case COUNT :
    for (i = 0; i < npts; i++)
    {
      ffld[i] = (float) ice[i].obs.count;
      cfld[i] = (unsigned char) ice[i].obs.count;
    }
    break;
  case HIRES_CONC :
    for (i = 0; i < npts; i++)
    {
      ffld[i] = (float) ice[i].obs.hires_conc/100.;
      cfld[i] = (unsigned char) ice[i].obs.hires_conc ;
    }
    break;
  default :
    return -1;
  }

  return 0;

}

/* Perform extended weather filtering (per OMB Tech Note 120) on
     SSMI data for sea ice use */
/* Robert Grumbine 4 June 1997 */
 
float gr37(grid2<ssmipt> &map, const int i, const int j, const int range) ;
float gr22(grid2<ssmipt> &map, const int i, const int j, const int range) ;

/* Variables for tracking which filters get used */
/* 16 March 2004 */
extern int efilt_37_n, efilt_37_s;
extern int efilt_22_n, efilt_22_s;

int newfilt(northgrid<ssmipt> &nmap, southgrid<ssmipt> &smap) {
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

  getfld(nmap, nconc, &g37[0][0], BAR_CONC);
  getfld(smap, sconc, &g37[0][0], BAR_CONC);

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
        if (g37[loc.j][loc.i] > GR37LIM) {
           efilt_37_n += 1;
           nconc[loc] = WEATHER;
        }
        if (g22[loc.j][loc.i] > GR22LIM) {
           efilt_22_n += 1;
           nconc[loc] = WEATHER;
        }

        if (nconc[indexjp1] != BAD_DATA) {
          if (g37[loc.j+1][loc.i] + g37[loc.j][loc.i] > 2*GR37LIM ) {
            efilt_37_n += 1;
            nconc[loc] = WEATHER;
            if (debug) printf("1 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (nconc[indexjm1] != BAD_DATA) {
          if (g37[loc.j-1][loc.i] + g37[loc.j][loc.i] > 2*GR37LIM ) {
            efilt_37_n += 1;
            nconc[loc] = WEATHER;
            if (debug) printf("2 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (nconc[indexm1] != BAD_DATA) {
          if (g37[loc.j][loc.i-1] + g37[loc.j][loc.i] > 2*GR37LIM ) {
            efilt_37_n += 1;
            nconc[loc] = WEATHER;
            if (debug) printf("3 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (nconc[indexp1] != BAD_DATA) {
          if (g37[loc.j][loc.i+1] + g37[loc.j][loc.i] > 2*GR37LIM ) {
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
        index = loc.i+loc.j*smap.xpoints();
        indexp1 = index + 1; 
        indexm1 = index - 1; 
        indexjm1 = index - smap.xpoints(); 
        indexjp1 = index - smap.xpoints(); 
        if (g37[loc.j][loc.i] > GR37LIM) {
           efilt_37_s += 1;
           sconc[loc] = WEATHER;
        }
        if (g22[loc.j][loc.i] > GR22LIM) {
           efilt_22_s += 1;
           sconc[loc] = WEATHER;
        }

        if (sconc[indexjp1] != BAD_DATA) {
          if (g37[loc.j+1][loc.i] + g37[loc.j][loc.i] > 2*GR37LIM ) {
            efilt_37_s += 1;
            sconc[loc] = WEATHER;
            if (debug) printf("1 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (sconc[indexjm1] != BAD_DATA) {
          if (g37[loc.j-1][loc.i] + g37[loc.j][loc.i] > 2*GR37LIM ) {
            efilt_37_s += 1;
            sconc[loc] = WEATHER;
            if (debug) printf("2 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (sconc[indexm1] != BAD_DATA) {
          if (g37[loc.j][loc.i-1] + g37[loc.j][loc.i] > 2*GR37LIM ) {
            efilt_37_s += 1;
            sconc[loc] = WEATHER;
            if (debug) printf("3 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (sconc[indexp1] != BAD_DATA) {
          if (g37[loc.j][loc.i+1] + g37[loc.j][loc.i] > 2*GR37LIM ) {
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
   
