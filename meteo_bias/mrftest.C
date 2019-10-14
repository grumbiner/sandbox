#include <stdio.h>
#include "ncepgrids.h"

template <class T>
class gaussian : public grid2<T> {
  public:
    gaussian(int n=126);
};
template <class T>
gaussian<T>::gaussian(int n) {
  nx = 3*n + 6;
  ny = (3*n+2)/2;
  grid = new T[nx*ny];
}

int avg(char *fname, char *base, int period) ;

int main(void) {
  gaussian<float> land, ice;
  gaussian<float> refland, moveland;
  gaussian<float> alwaysice, neverice, avgice;
  FILE *fland, *fice;
  int i, steps, sumall, sumnever, period;
  ijpt loc;
  char fname[90];
  
  fland = fopen("LAND","r");
  fice  = fopen("ICEC","r");
  
  if (fland == (FILE *) NULL) {
    printf("Failed to open the land file\n");
    return 1;
  }
  if (fice == (FILE *) NULL) {
    printf("Failed to open the ice file\n");
    return 2;
  }

  land.binin(fland);
  ice.binin(fice);
  refland = land;
  moveland.set(0.);
  alwaysice = ice;
  neverice  = ice; 
  avgice    = ice;
//Note that the -1 is erroneous, covering another error.  There seems to
//  be one too few input files
  for (i = 1; i <= 1459; i++) {
    land.binin(fland);
    ice.binin(fice);
    avgice += ice;
    steps += 1;

    for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
      // Check that land doesn't change from initial:
      if (land[loc] != refland[loc]) {
        moveland[loc] = 1.;
        printf("step %d point %3d %3d differs from reference %f %f\n",
          i, loc.i, loc.j, refland[loc], land[loc]);
      }

      // Check for the ice cover:
      if (ice[loc] == 0.) alwaysice[loc] = 0.;
      if (ice[loc] == 1.) neverice[loc] = 1.; 
      if (ice[loc] != 0. && ice[loc] != 1.) {
        printf("odd ice value %d %3d %3d %f\n",i, loc.i, loc.j, ice[loc]);
      }
    }
    }
  }

  sumall = 0;
  sumnever = 0;
  for (loc.j = 0; loc.j < avgice.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < avgice.xpoints(); loc.i++) {
     //printf("%3d %3d  %f %1d %1d\n",loc.i, loc.j, avgice[loc], 
     //      (int) alwaysice[loc], (int) neverice[loc]);
     if (alwaysice[loc] == 1.) sumall += 1;
     if (neverice[loc] == 0.) sumnever += 1;
  }
  }
  steps = i-1;
  avgice /= (float) (steps);
  printf("Always ice covered: %d \n Never ice covered: %d\n",sumall, sumnever);

  loc.i = 1; loc.j = 1;
  printf("Land flag, NP %f\n",refland[loc]);
  loc.i = refland.xpoints()-1; loc.j = refland.ypoints()-1;
  printf("Land flag, SP %f\n",refland[loc]);
  

// Now perform some _a_posteriori_ checks on where odd things are
//   happening:
  rewind(fice);
  for (i = 0; i < steps ; i++) {
    ice.binin(fice);
    for (loc.j = 0; loc.j < avgice.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < avgice.xpoints(); loc.i++) {
      if (avgice[loc] >= 1. - 3./steps && ice[loc] != 1.) {
        printf("anomalous ice %4d %3d %3d avg is %f value %f\n",
            i, loc.i, loc.j, avgice[loc], ice[loc]);
      }
      if (avgice[loc] <= 3./steps && ice[loc] != 0.) {
        printf("anomalous ice %4d %3d %3d avg is %f value %f\n",
            i, loc.i, loc.j, avgice[loc], ice[loc]);
      }
    }
    }
  }

// Note that avgice != 0.0 -> areas with some ice cover.
  

//////////////////////////////////////////
// Start looking at physical parameters:
  period = 2;
  sprintf(fname,"lhavg.%d",period);
  i = avg("LHTFL", fname, period);
  sprintf(fname,"shavg.%d",period);
  i = avg("SHTFL", fname, period);
  sprintf(fname,"albedoavg.%d",period);
  i = avg("ALBDO", fname, period);
  sprintf(fname,"snodepavg.%d",period);
  i = avg("WEASD", fname, period);

  period = 4;
  sprintf(fname,"lhavg.%d",period);
  i = avg("LHTFL", fname, period);
  sprintf(fname,"shavg.%d",period);
  i = avg("SHTFL", fname, period);
  sprintf(fname,"albedoavg.%d",period);
  i = avg("ALBDO", fname, period);
  sprintf(fname,"snodepavg.%d",period);
  i = avg("WEASD", fname, period);

  period = 20;
  sprintf(fname,"lhavg.%d",period);
  i = avg("LHTFL", fname, period);
  sprintf(fname,"shavg.%d",period);
  i = avg("SHTFL", fname, period);
  sprintf(fname,"albedoavg.%d",period);
  i = avg("ALBDO", fname, period);
  sprintf(fname,"snodepavg.%d",period);
  i = avg("WEASD", fname, period);

  return 0;
}


int avg(char *fname, char *base, int period) {
  gaussian<float> in, avg;
  FILE *fin, *fout;
  int i;

  fin = fopen(fname, "r");
  fout = fopen(base, "w");
  if (fin == (FILE *) NULL) {
    printf("Failed to open the input file %s\n",fname);
    return 1;
  }
  if (fout == (FILE *) NULL) {
    printf("Failed to open the output file %s\n",base);
    return 2;
  }
  in.binin(fin);
  avg = in;
  for (i = 2; i <= 1460; i++) {
    in.binin(fin);
    avg += in;
    if ( (i%period) == 0) {
      avg /= (float) period;
      avg.binout(fout);
    }
  }

  fclose(fout);
  fclose(fin);
  return 0;
}

int avg(char *fname, char *base, int period, FILE *ice, FILE *land) {
  gaussian<float> ifland, ifocean, ifice;
  gaussian<float> icein, landin, parmin;
  FILE *fin, *fout;
  int i;
  ijpt loc;

  rewind(ice);
  rewind(land);
  fin = fopen(fname,"r");
  fout = fopen(base,"w");
  if (fin == (FILE *) NULL) {
    printf("Failed to open the input file %s\n",fname);
    return 1;
  }
  if (fout == (FILE *) NULL) {
    printf("Failed to open the output file %s\n",base);
    return 2;
  }

  for (i = 0; i < 1460; i++) {
    parmin.binin(fin);
    landin.binin(land);
    icein.binin(ice);
    for (loc.j = 0; loc.j < parmin.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < parmin.xpoints(); loc.i++) {
       if (landin[loc] == 1.) {
         ifland[loc] += parmin[loc];
       }
       else if (icein[loc] == 0.) {
         ifocean[loc] += parmin[loc];
       }
       else if (icein[loc] == 1.) {
         ifice[loc] += parmin[loc];
       }
       else {
         printf("error, impossible ice concentration at %d %d  %f\n",
             loc.i, loc.j, icein[loc]);
       }
    }
    }
  }

  return 0;
}
    






//416256 -rw-r--r--   1 wx21rg   g01      426086400 Jun 23 12:15 USWRF
//416256 -rw-r--r--   1 wx21rg   g01      426086400 Jun 23 12:15 DSWRF
//416256 -rw-r--r--   1 wx21rg   g01      426086400 Jun 23 11:13 GFLUX
//416256 -rw-r--r--   1 wx21rg   g01      426086400 Jun 22 16:53 ALBDO
//416104 -rw-r--r--   1 wx21rg   g01      426086400 Jun 23 12:15 DLWRF
//416256 -rw-r--r--   1 wx21rg   g01      426086400 Jun 23 12:15 ULWRF
