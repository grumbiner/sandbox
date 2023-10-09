#include "ncepgrids.h"
#include "icessmi.h"

#define MAXICE 127 
#define MAXAGE  16 

/* Construct a filled lat-long grid of sea ice data from yesterday's
     filled map and today's observations.  If today has a valid 
     observation, use it.  If there is no valid observation today,
     use the value from yesterday's map and increment the age.
     If the age reaches MAXAGE (i.e., MAXAGE days with no valid observations),
     set the ice concentration to zero and restart the age counter.
  Robert Grumbine 4 June 1997 */ 
// Updated comments, move to a compile-defined GRIDTYPE, and cast NULL to
//   (FILE *).  Robert Grumbine 29 June 2006

template <class T>
int isolani(metricgrid<T> &x); // remove points which are surrounded by
                               // zeroes
/* Argument for size, with malloc of the arrays? */
int main(int argc, char *argv[])
{
  FILE *fiage, *foage, *fice1, *fice2, *foice;
  GRIDTYPE<unsigned char> iage, oage, ice1, ice2;
  GRIDTYPE<unsigned char> oice;
  ijpt x;

  fice1 = fopen(argv[1], "r");
  fice2 = fopen(argv[2], "r");
  fiage = fopen(argv[3], "r");
  foage = fopen(argv[4], "w");
  foice = fopen(argv[5], "w");

// If no old age file, correct and carry on 26 April 2004
  if ( fice2 == (FILE *) NULL ) {
    printf("Failed to open a required input file!\n");
    if (fice2 == (FILE *) NULL ) { printf("Failed to open new latlon ice file\n"); }
    return -1;
  }
  if (foage == (FILE *) NULL || foice == (FILE *) NULL ) {
    printf("Failed to open a required output file!\n");
    return -1;
  }

  ice2.binin(fice2);
  isolani(ice2);  // remove isolated ice points

  if (fice1 == (FILE *) NULL) {
    ice1.set((unsigned char) 224);
  }
  else {
    ice1.binin(fice1);
  }
  #ifdef VERBOSE2
    printf("Yesterday max min %d %d Today max min %d %d\n",
            ice1.gridmax(), ice1.gridmin(), ice2.gridmax(), ice2.gridmin() );
  #endif

  if (fiage == (FILE *) NULL) {
    iage.set((unsigned char) MAXAGE - 1);
  }
  else {
    iage.binin(fiage);
  }

//Do not need to scale the ice concentrations, because the global fields
//  being input are guaranteed (unsigned char) to be in percents, rather
//  than fractions.

  for (x.j = 0; x.j < ice1.ypoints() ; x.j++) {
    for (x.i = 0; x.i < ice1.xpoints() ; x.i++) {
      if (ice2[x] >= ( (unsigned char) MAXICE ) ) { 
        if (ice2[x] == (unsigned char) WEATHER ) {
          oice[x] = 0;
          oage[x] = 0;
        }
        else {
          oice[x] = ice1[x];
          oage[x] = iage[x] + 1;
        }
      }
      else {
        oice[x] = ice2[x];
        oage[x] = 0;
      }
      if (oice[x] > MAXICE) {
        oice[x] = 100;
      }
      else if (oice[x] < MIN_CONC) {
        oice[x] = 0;
      }
      if (iage[x] > MAXAGE ) { 
        #ifdef VERBOSE
         if (oice[x] != 0) {
           printf("Reset point %d %d for overage %d from conc. %d\n",
            x.i, x.j, oage[x], (int) oice[x]);
          }
        #endif 
        oice[x] = 0; 
        oage[x] = 0; 
      }

    }
  }

  oice.binout(foice);
  oage.binout(foage);

  return 0;

}
template <class T>
int isolani(metricgrid<T> &x) {
  int count = 0;
  ijpt loc, ip1, jp1, im1, jm1 ;
  ijpt  tip1, tjp1, tim1, tjm1 ;

  ip1.i =  1; ip1.j = 0;
  im1.i = -1; ip1.j = 0;
  jp1.i =  0; jp1.j =  1;
  jm1.i =  0; jm1.j = -1;

  for (loc.j = 1; loc.j < x.ypoints() - 1; loc.j++) {
  for (loc.i = 1; loc.i < x.xpoints() - 1; loc.i++) {
     if (x[loc] != 0) {
       tip1 = loc ; tip1 += ip1;
       tim1 = loc ; tim1 += im1;
       tjp1 = loc ; tjp1 += jp1;
       tjm1 = loc ; tjm1 += jm1;
       if (x[tip1] == 0 && x[tim1] == 0 && x[tjp1] == 0 && x[tjm1] == 0) {
         x[loc] = 0;
         count += 1;
       } 
     }
  }
  }
  printf("%d isolated points removed\n",count);
  return count;
}
