#include "ncepgrids.h"

int main(void) {
  global_ice<unsigned char> prior, newer, countl, countc, countw;
  global_12th<unsigned char> hr;
  FILE *fin, *fout;

  fin = fopen("seaice_newland","r");
  prior.binin(fin);
  fclose(fin);
  fin = fopen("seaice_gland5min","r");
  hr.binin(fin);
  fclose(fin);

  ijpt loc1, loc2;
  latpt ll;

  countl.set(0);
  countc.set(0);
  countw.set(0);

  for (loc1.j = 0; loc1.j < hr.ypoints(); loc1.j++) {
  for (loc1.i = 0; loc1.i < hr.xpoints(); loc1.i++) {
    ll = hr.locate(loc1);
    loc2 = countl.locate(ll);

    if (hr[loc1] == LAND) {
      countl[loc2] += 1;
    }
    else if (hr[loc1] != 0) {
      if (hr[loc1] == 195) {
        countc[loc2] += 1;
      }
      else {
        printf("hr = %d\n",hr[loc1]);
      }
    }
    else {
      countw[loc2] += 1;
    }
  }
  } 

  printf("countl %d %d\n",countl.gridmax(), countl.gridmin());
  printf("countw %d %d\n",countw.gridmax(), countw.gridmin());
  printf("countc %d %d\n",countc.gridmax(), countc.gridmin());

  newer.set(224);

  for (loc2.j = 0; loc2.j < countl.ypoints(); loc2.j++) {
  for (loc2.i = 0; loc2.i < countl.xpoints(); loc2.i++) {
// If majority is this type, use it:
    if (countl[loc2] > 18) {
      newer[loc2] = LAND;
    }
    //else if (countc[loc2] > 18) {
    //  newer[loc2] = COAST;
    //}
    else if (countw[loc2] > 18) {
      newer[loc2] = 0;
    }
// If more land than water, use that, likewise if more water than land:
    else if (countl[loc2] > countw[loc2]) {
      newer[loc2] = LAND;
    }
    else if (countw[loc2] > countl[loc2]) {
      newer[loc2] = 0;
    }
    // neither majority land/water, nor one greater than others
    else if (countc[loc2] > 18) {
      newer[loc2] = COAST;
    }

// Else: punt
    else {
      ll = countl.locate(loc2);
      printf("not easy at %3d %3d  %6.2f %6.2f  %2d %2d %2d\n",loc2.i, loc2.j, 
             ll.lat, ll.lon, countl[loc2], countc[loc2], countw[loc2]);
    }

// if not the same, print out:
    if (newer[loc2] != prior[loc2]) {
      ll = countl.locate(loc2);
      printf("%3d %3d  %6.2f %6.2f  old %3d newer %3d\n",loc2.i, loc2.j, 
             ll.lat, ll.lon, prior[loc2], newer[loc2]);
    }

  }
  }

  return 0;
}
