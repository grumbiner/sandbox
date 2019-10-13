#include "ncepgrids.h"
//Produce a plain text map centered on a given lat-lon
//  (demo is for 70 S, 0 E)
//From both the low (30 arc minute) and high (5 arc minute) resolution
//  analyses
//Robert Grumbine 9 August 2006
//
//The origin of this was a 1990s field experiment by Don Cavalieri,
//  bandwidth being too limited for sending graphics

void ascii(llgrid<float> &ice, ijpt &loc, FILE *fout) ;

int main(int argc, char *argv[]) {
  global_ice<float> half;
  global_12th<float> high;
  FILE *fin, *fout;
  latpt ll;
  ijpt loc;

  fin = fopen(argv[1],"r");
  half.binin(fin);
  fclose(fin);

  fin = fopen(argv[2],"r");
  high.binin(fin);
  fclose(fin);

  ll.lat = -70.0;
  ll.lon = 0.0;

  fout = fopen(argv[3], "w");
  loc = half.locate(ll);
  ascii(half, loc, fout);
  fclose(fout);

  fout = fopen(argv[4], "w");
  loc = high.locate(ll);
  ascii(high, loc, fout);
  fclose(fout);

  return 0;
}


void ascii(llgrid<float> &ice, ijpt &refloc, FILE *fout) {
  grid2_base<char> omap(79, 59);
  int iref, jref, imin, imax, jmin, jmax;
  ijpt loc, tloc;
  int x;
  char y;
  int i, j;

// Set up values:
  iref = refloc.i; jref = refloc.j;
  imin = iref - 37;
  imax = iref + 37;

  jmin = jref - 27;
  jmax = jref + 27;
  if (imin < 0 ) imin = 0;
  if (jmin < 0 ) jmin = 0;
  if (imax > ice.xpoints() - 1) imax = ice.xpoints() - 1;
  if (jmax > ice.ypoints() - 1) jmax = ice.ypoints() - 1;

// Initialize the output map:
  for (tloc.j = 0; tloc.j < omap.ypoints() ; tloc.j++) { 
  for (tloc.i = 0; tloc.i < omap.xpoints() ; tloc.i++) { 
    omap[tloc] = 'B' ;
  }
  }

/* Now begin the character remapping of the sub-field */
  fprintf(fout, "starting map ^L\n"); fflush(stdout);

  fprintf(fout, "     ");
  for (i = imin ; i <= imax ; i++) { 
    fprintf(fout, "%01d", (i ) / 1000 ) ;
  }
  fprintf(fout, "\n");

  fprintf(fout, "     ");
  for (i = imin ; i <= imax ; i++) { 
    fprintf(fout, "%01d", ((i ) / 100) % 100 ) ;
  }
  fprintf(fout, "\n");

  fprintf(fout, "     ");
  for (i = imin ; i <= imax ; i++) { 
    fprintf(fout, "%01d", ( (i ) / 10) % 10         ) ;
  }
  fprintf(fout, "\n");
  fprintf(fout, "     ");
  for (i = imin ; i <= imax ; i++) { 
    fprintf(fout, "%01d",  (i ) % 10         ) ;
  }
  fprintf(fout, "\n");

  printf("ready to start working on j\n"); fflush(stdout);

  //for (loc.j = jmax; loc.j >= jmin ; loc.j-- ) {
  //   tloc.j = jmax - loc.j;
  // don't need to flip the lat-long grids
  for (loc.j = jmin; loc.j <= jmax ; loc.j++ ) {
     tloc.j = loc.j - jmin;
     fprintf(fout, "%4d ", loc.j ) ;
     for (loc.i = imin; loc.i <= imax ; loc.i++) {
        tloc.i = loc.i - imin;
        y = '.';
        x = (int) (0.5 + ice[loc]*100.);
        if (  x == LAND )    y = 'L';
        if (  x == COAST )   y = 'C';
        if (  x == NO_DATA)  y = 'N';
        if (  x == BAD_DATA) y = 'B';
        if (  x >= 100 && x < LAND) x = 99;
        if (  x < 100 ) sprintf(&y,"%1d",(int) ( x / 10 ) ) ;

        omap[tloc] = y ;
        fprintf(fout, "%c",y);
     }
     fprintf(fout, "\n");
  }

  return;
}
